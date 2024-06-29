open Core

let string_table =
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!"
  ^ "\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n"

let decode_string input = String.map input ~f:(fun c -> string_table.[Char.to_int c - 33])

let decode_token input =
  match input.[0] with
  | 'S' ->
      (* Decode the string *)
      let input = String.sub input ~pos:1 ~len:(String.length input - 1) in
      decode_string input
  | _ -> failwith "Not supported yet"

let inverted_table =
  let table = Array.create ~len:256 '@' in
  String.iteri string_table ~f:(fun i c -> table.(Char.to_int c) <- Char.of_int_exn (i + 33));
  table

let encode_string input = String.map input ~f:(fun c -> inverted_table.(Char.to_int c))
let encode_string_token input = "S" ^ encode_string input

type unop = Minus | Not | StringToInt | IntToString [@@deriving sexp, equal, compare]

type binop =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Less
  | Greater
  | Equal
  | And
  | Or
  | StringConcat
  | Take
  | Drop
  | Apply
[@@deriving sexp, equal, compare]

type term =
  | Boolean of bool
  | Integer of Bigint.t
  | String of string
  | Unary of unop * term
  | Binary of binop * term * term
  | If of term * term * term
  | Abstract of int * term
  | Var of int
[@@deriving sexp, equal, compare]

let big (i : int) = Bigint.of_int i
let small (i : Bigint.t) = Bigint.to_int_exn i
let ( ++ ) = Bigint.( + )
let ( -- ) = Bigint.( - )
let ( ** ) = Bigint.( * )
let ( // ) = Bigint.( / )

let quo_rem (a : Bigint.t) (b : Bigint.t) =
  let quo = Bigint.( / ) a b in
  let rem = a -- (b ** quo) in
  (quo, rem)

let parse_int (s : string) : int =
  (* Look up the index of each char of s in string_table *)
  let indexes = String.to_list s |> List.map ~f:(fun c -> Char.to_int c - 33) in
  (* Convert the indexes to a single integer as base 94 *)
  List.fold indexes ~init:0 ~f:(fun acc i -> (acc * 94) + i)

let parse_big_int (s : string) : Bigint.t =
  (* Look up the index of each char of s in string_table *)
  let indexes = String.to_list s |> List.map ~f:(fun c -> Char.to_int c - 33) in
  (* Convert the indexes to a single integer as base 94 *)
  List.fold indexes ~init:Bigint.zero ~f:(fun acc i -> (acc ** big 94) ++ big i)

let parse (input : string) : term =
  let tokens = String.split ~on:' ' input in
  let rec get_term tokens =
    (* Read the next term by parsing from left to right *)
    match tokens with
    | first_token :: rest -> (
        let first_char = first_token.[0] in
        let body = String.sub first_token ~pos:1 ~len:(String.length first_token - 1) in
        match first_char with
        | 'T' -> (Boolean true, rest)
        | 'F' -> (Boolean false, rest)
        | 'I' -> (Integer (parse_big_int body), rest)
        | 'S' -> (String (decode_string body), rest)
        | 'U' ->
            let operand, rest = get_term rest in
            let out_term =
              match body with
              | "-" -> Unary (Minus, operand)
              | "!" -> Unary (Not, operand)
              | "#" -> Unary (StringToInt, operand)
              | "$" -> Unary (IntToString, operand)
              | _ -> failwith (sprintf "Unexpected unary operator: %s" body)
            in
            (out_term, rest)
        | 'B' ->
            let first_op, rest = get_term rest in
            let second_op, rest = get_term rest in
            let out_term =
              match body with
              | "+" -> Binary (Add, first_op, second_op)
              | "-" -> Binary (Sub, first_op, second_op)
              | "*" -> Binary (Mul, first_op, second_op)
              | "/" -> Binary (Div, first_op, second_op)
              | "%" -> Binary (Mod, first_op, second_op)
              | "<" -> Binary (Less, first_op, second_op)
              | ">" -> Binary (Greater, first_op, second_op)
              | "=" -> Binary (Equal, first_op, second_op)
              | "&" -> Binary (And, first_op, second_op)
              | "|" -> Binary (Or, first_op, second_op)
              | "." -> Binary (StringConcat, first_op, second_op)
              | "T" -> Binary (Take, first_op, second_op)
              | "D" -> Binary (Drop, first_op, second_op)
              | "$" -> Binary (Apply, first_op, second_op)
              | _ -> failwith (sprintf "Unexpected binary operator: %s" body)
            in
            (out_term, rest)
        | '?' ->
            assert (String.is_empty body);
            let cond, rest = get_term rest in
            let then_, rest = get_term rest in
            let else_, rest = get_term rest in
            (If (cond, then_, else_), rest)
        | 'L' ->
            let var = parse_int body in
            let expr, rest = get_term rest in
            (Abstract (var, expr), rest)
        | 'v' ->
            let var = parse_int body in
            (Var var, rest)
        | _ -> failwith (sprintf "Unexpected token: %s" first_token))
    | [] -> failwith "Unexpected end of input"
  in
  let term, rest = get_term tokens in
  assert (List.is_empty rest);
  term

let deparse_big_int (i : Bigint.t) : string =
  if Bigint.compare i (big 0) < 0 then failwith "Negative integers are not supported";
  if Bigint.compare i (big 0) = 0 then "!"
  else
    let rec get_digits i =
      if Bigint.( = ) i (big 0) then []
      else
        let q, r = quo_rem i (big 94) in
        small r :: get_digits q
    in
    let digits = get_digits i in
    let chars = List.map digits ~f:(fun i -> Char.of_int_exn (i + 33)) in
    String.of_char_list (List.rev chars)

let deparse_int (i : int) : string = deparse_big_int (big i)

let rec deparse (prg : term) =
  match prg with
  | Boolean b -> if b then "T" else "F"
  | Integer i -> "I" ^ deparse_big_int i
  | String s -> encode_string_token s
  | Unary (op, t) ->
      let op_str =
        match op with
        | Minus -> "U-"
        | Not -> "U!"
        | StringToInt -> "U#"
        | IntToString -> "U$"
      in
      op_str ^ " " ^ deparse t
  | Binary (op, t1, t2) ->
      let op_str =
        match op with
        | Add -> "B+"
        | Sub -> "B-"
        | Mul -> "B*"
        | Div -> "B/"
        | Mod -> "B%"
        | Less -> "B<"
        | Greater -> "B>"
        | Equal -> "B="
        | And -> "B&"
        | Or -> "B|"
        | StringConcat -> "B."
        | Take -> "BT"
        | Drop -> "BD"
        | Apply -> "B$"
      in
      op_str ^ " " ^ deparse t1 ^ " " ^ deparse t2
  | If (cond, then_, else_) -> "? " ^ deparse cond ^ " " ^ deparse then_ ^ " " ^ deparse else_
  | Abstract (var, body) -> "L" ^ deparse_int var ^ " " ^ deparse body
  | Var var -> "v" ^ deparse_int var

let parens ctx_predence op_precedence str =
  if ctx_predence <= op_precedence then "(" ^ str ^ ")" else str

let rec pp_as_lambda ctx_precedence term =
  match term with
  | Boolean b -> if b then "true" else "false"
  | Integer i -> Bigint.to_string_hum i
  | String s -> "\"" ^ s ^ "\""
  | Unary (op, t) ->
      let op_str =
        match op with
        | Minus -> "-"
        | Not -> "!"
        | StringToInt -> "string_to_int"
        | IntToString -> "int_to_string"
      in
      op_str ^ pp_as_lambda 1 t
  | Binary (Apply, t1, t2) ->
      let p = 5 in
      parens ctx_precedence p (pp_as_lambda p t1 ^ " " ^ pp_as_lambda p t2)
  | Binary (op, t1, t2) ->
      let op_str, p =
        match op with
        | Add -> ("+", 15)
        | Sub -> ("-", 15)
        | Mul -> ("*", 10)
        | Div -> ("/", 10)
        | Mod -> ("%", 10)
        | Less -> ("<", 20)
        | Greater -> (">", 20)
        | Equal -> ("=", 20)
        | And -> ("&&", 25)
        | Or -> ("||", 30)
        | StringConcat -> ("^", 15)
        | _ -> failwith "Not supported yet"
      in
      parens ctx_precedence p (pp_as_lambda p t1 ^ " " ^ op_str ^ " " ^ pp_as_lambda p t2)
  | If (cond, then_, else_) ->
      "if "
      ^ pp_as_lambda 50 cond
      ^ " then "
      ^ pp_as_lambda 50 then_
      ^ " else "
      ^ pp_as_lambda 50 else_
      ^ " fi"
  | Abstract (var, body) ->
      let p = 50 in
      parens ctx_precedence p ("\\v" ^ Int.to_string var ^ " -> " ^ pp_as_lambda p body)
  | Var var -> "v" ^ Int.to_string var

(* TESTS *)
let%test_unit "quo_rem" = [%test_eq: Bigint.t * Bigint.t] (quo_rem (big 10) (big 3)) (big 3, big 1)

let%test_unit "encode_string" =
  [%test_eq: string] (encode_string "Hello World!") "B%,,/}Q/2,$_";
  [%test_eq: string] (encode_string " World!") "}Q/2,$_"

let%test_unit "decode_string" = [%test_eq: string] (decode_string "}Q/2,$_") " World!"
let%test_unit "parse_int" = [%test_eq: int] (parse_int "/6") 1337
let%test_unit "deparse_int" = [%test_eq: string] (deparse_int 1337) "/6"

let%test_unit "literals" =
  [%test_eq: term] (parse "T") (Boolean true);
  [%test_eq: term] (parse "F") (Boolean false);
  [%test_eq: term] (parse "I/6") (Integer (big 1337));
  [%test_eq: term] (parse "SB%,,/") (String "Hello");
  [%test_eq: term] (parse "U- I$") (Unary (Minus, Integer (big 3)));
  [%test_eq: term] (parse "U! T") (Unary (Not, Boolean true));
  [%test_eq: term] (parse "U# S4%34") (Unary (StringToInt, String "test"));
  [%test_eq: term] (parse "U$ I4%34") (Unary (IntToString, Integer (big 15818151)));
  [%test_eq: term] (parse "B+ I# I$") (Binary (Add, Integer (big 2), Integer (big 3)));
  [%test_eq: term] (parse "B- I$ I#") (Binary (Sub, Integer (big 3), Integer (big 2)));
  [%test_eq: term] (parse "B* I$ I#") (Binary (Mul, Integer (big 3), Integer (big 2)));
  [%test_eq: term] (parse "B/ U- I( I#")
    (Binary (Div, Unary (Minus, Integer (big 7)), Integer (big 2)));
  [%test_eq: term] (parse "B% U- I( I#")
    (Binary (Mod, Unary (Minus, Integer (big 7)), Integer (big 2)));
  [%test_eq: term] (parse "B< I$ I#") (Binary (Less, Integer (big 3), Integer (big 2)));
  [%test_eq: term] (parse "B> I$ I#") (Binary (Greater, Integer (big 3), Integer (big 2)));
  [%test_eq: term] (parse "B= I$ I#") (Binary (Equal, Integer (big 3), Integer (big 2)));
  [%test_eq: term] (parse "B& T F") (Binary (And, Boolean true, Boolean false));
  [%test_eq: term] (parse "B| T F") (Binary (Or, Boolean true, Boolean false));
  [%test_eq: term] (parse "B. S4% S34") (Binary (StringConcat, String "te", String "st"));
  [%test_eq: term] (parse "BT I$ S4%34") (Binary (Take, Integer (big 3), String "test"));
  [%test_eq: term] (parse "BD I$ S4%34") (Binary (Drop, Integer (big 3), String "test"));
  [%test_eq: term] (parse "? B> I# I$ S9%3 S./")
    (If (Binary (Greater, Integer (big 2), Integer (big 3)), String "yes", String "no"));
  [%test_eq: term] (parse "L/6 I$") (Abstract (1337, Integer (big 3)));
  [%test_eq: term] (parse "v/6") (Var 1337);
  [%test_eq: term]
    (parse "B$ B$ L# L$ v# B. SB%,,/ S}Q/2,$_ IK")
    (Binary
       ( Apply,
         Binary
           ( Apply,
             Abstract (2, Abstract (3, Var 2)),
             Binary (StringConcat, String "Hello", String " World!") ),
         Integer (big 42) ))

let%test_unit "deparse_int" = [%test_eq: string] (deparse_int 0) "!"
let%test_unit "deparse_int" = [%test_eq: string] (deparse_int 1337) "/6"

let%test_unit "deparse" =
  [%test_eq: string] (parse "T" |> deparse) "T";
  [%test_eq: string] (parse "F" |> deparse) "F";
  [%test_eq: string] (parse "I/6" |> deparse) "I/6";
  [%test_eq: string] (parse "SB%,,/" |> deparse) "SB%,,/";
  [%test_eq: string] (parse "U- I$" |> deparse) "U- I$";
  [%test_eq: string] (parse "U! T" |> deparse) "U! T";
  [%test_eq: string] (parse "U# S4%34" |> deparse) "U# S4%34";
  [%test_eq: string] (parse "U$ I4%34" |> deparse) "U$ I4%34";
  [%test_eq: string] (parse "B+ I# I$" |> deparse) "B+ I# I$";
  [%test_eq: string] (parse "B- I$ I#" |> deparse) "B- I$ I#";
  [%test_eq: string] (parse "B* I$ I#" |> deparse) "B* I$ I#";
  [%test_eq: string] (parse "B/ U- I( I#" |> deparse) "B/ U- I( I#";
  [%test_eq: string] (parse "B% U- I( I#" |> deparse) "B% U- I( I#";
  [%test_eq: string] (parse "B< I$ I#" |> deparse) "B< I$ I#";
  [%test_eq: string] (parse "B> I$ I#" |> deparse) "B> I$ I#";
  [%test_eq: string] (parse "B= I$ I#" |> deparse) "B= I$ I#";
  [%test_eq: string] (parse "B& T F" |> deparse) "B& T F";
  [%test_eq: string] (parse "B| T F" |> deparse) "B| T F";
  [%test_eq: string] (parse "B. S4% S34" |> deparse) "B. S4% S34";
  [%test_eq: string] (parse "BT I$ S4%34" |> deparse) "BT I$ S4%34";
  [%test_eq: string] (parse "BD I$ S4%34" |> deparse) "BD I$ S4%34";
  [%test_eq: string] (parse "? B> I# I$ S9%3 S./" |> deparse) "? B> I# I$ S9%3 S./";
  [%test_eq: string] (parse "L/6 I$" |> deparse) "L/6 I$";
  [%test_eq: string] (parse "v/6" |> deparse) "v/6";
  [%test_eq: string]
    (parse "B$ B$ L# L$ v# B. SB%,,/ S}Q/2,$_ IK" |> deparse)
    "B$ B$ L# L$ v# B. SB%,,/ S}Q/2,$_ IK";
  [%test_eq: string]
    (parse
       ("B$ B$ L\" B$ L\" B$ L# B$ v\" B$ v# v# L# B$ v\" B$ v# v# L$ L# ? "
      ^ "B= v# I\" v\" B. v\" B$ v$ B- v# I\" SL I#,")
    |> deparse)
    ("B$ B$ L\" B$ L\" B$ L# B$ v\" B$ v# v# L# B$ v\" B$ v# v# L$ L# ? "
   ^ "B= v# I\" v\" B. v\" B$ v$ B- v# I\" SL I#,")
