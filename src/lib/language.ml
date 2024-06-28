open Core

let string_table =
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!"
  ^ "\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n"

let decode_string input = String.map input ~f:(fun c -> string_table.[Char.to_int c - 33])

let decode_token input =
  (* Trim whitespace from input *)
  let input = String.strip input in
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

let encode_string input =
  let input = String.strip input in
  let input = String.map input ~f:(fun c -> inverted_table.(Char.to_int c)) in
  "S" ^ input

type unop = Minus | Not | StringToInt | IntToString [@@deriving sexp, compare]

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
[@@deriving sexp, compare]

type term =
  | Boolean of bool
  | Integer of int
  | String of string
  | Unary of unop * term
  | Binary of binop * term * term
  | If of term * term * term
  | Abstract of int * term
  | Var of int
[@@deriving sexp, compare]

let parse_int (s : string) : int =
  (* Look up the index of each char of s in string_table *)
  let indexes = String.to_list s |> List.map ~f:(fun c -> Char.to_int c - 33) in
  (* Convert the indexes to a single integer as base 94 *)
  List.fold indexes ~init:0 ~f:(fun acc i -> (acc * 94) + i)

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
        | 'I' -> (Integer (parse_int body), rest)
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

(* TESTS *)

let%test_unit "decode_string" = [%test_eq: string] (decode_string "}Q/2,$_") " World!"
let%test_unit "parse_int" = [%test_eq: int] (parse_int "/6") 1337

let%test_unit "literals" =
  [%test_eq: term] (parse "T") (Boolean true);
  [%test_eq: term] (parse "F") (Boolean false);
  [%test_eq: term] (parse "I/6") (Integer 1337);
  [%test_eq: term] (parse "SB%,,/") (String "Hello");
  [%test_eq: term] (parse "U- I$") (Unary (Minus, Integer 3));
  [%test_eq: term] (parse "U! T") (Unary (Not, Boolean true));
  [%test_eq: term] (parse "U# S4%34") (Unary (StringToInt, String "test"));
  [%test_eq: term] (parse "U$ I4%34") (Unary (IntToString, Integer 15818151));
  [%test_eq: term] (parse "B+ I# I$") (Binary (Add, Integer 2, Integer 3));
  [%test_eq: term] (parse "B- I$ I#") (Binary (Sub, Integer 3, Integer 2));
  [%test_eq: term] (parse "B* I$ I#") (Binary (Mul, Integer 3, Integer 2));
  [%test_eq: term] (parse "B/ U- I( I#") (Binary (Div, Unary (Minus, Integer 7), Integer 2));
  [%test_eq: term] (parse "B% U- I( I#") (Binary (Mod, Unary (Minus, Integer 7), Integer 2));
  [%test_eq: term] (parse "B< I$ I#") (Binary (Less, Integer 3, Integer 2));
  [%test_eq: term] (parse "B> I$ I#") (Binary (Greater, Integer 3, Integer 2));
  [%test_eq: term] (parse "B= I$ I#") (Binary (Equal, Integer 3, Integer 2));
  [%test_eq: term] (parse "B& T F") (Binary (And, Boolean true, Boolean false));
  [%test_eq: term] (parse "B| T F") (Binary (Or, Boolean true, Boolean false));
  [%test_eq: term] (parse "B. S4% S34") (Binary (StringConcat, String "te", String "st"));
  [%test_eq: term] (parse "BT I$ S4%34") (Binary (Take, Integer 3, String "test"));
  [%test_eq: term] (parse "BD I$ S4%34") (Binary (Drop, Integer 3, String "test"));
  [%test_eq: term] (parse "? B> I# I$ S9%3 S./")
    (If (Binary (Greater, Integer 2, Integer 3), String "yes", String "no"));
  [%test_eq: term] (parse "L/6 I$") (Abstract (1337, Integer 3));
  [%test_eq: term] (parse "v/6") (Var 1337);
  [%test_eq: term]
    (parse "B$ B$ L# L$ v# B. SB%,,/ S}Q/2,$_ IK")
    (Binary
       ( Apply,
         Binary
           ( Apply,
             Abstract (2, Abstract (3, Var 2)),
             Binary (StringConcat, String "Hello", String " World!") ),
         Integer 42 ))
