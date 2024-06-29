open Core
open Language

(** Is the variable free in the term *)
let is_free (var : int) (prg : term) : bool =
  let rec is_free' (prg : term) : bool =
    match prg with
    | Boolean _ -> false
    | Integer _ -> false
    | String _ -> false
    | Unary (_, t) -> is_free' t
    | Binary (_, t1, t2) -> is_free' t1 || is_free' t2
    | If (t1, t2, t3) -> is_free' t1 || is_free' t2 || is_free' t3
    | Abstract (x, t) -> if x = var then false else is_free' t
    | Var x -> x = var
    | Thunk t -> is_free' !t
  in
  is_free' prg

(** The maximal variable number used anywhere in the program (free or not) *)
let max_var (prg : term) : int =
  let rec max_var' (prg : term) : int =
    match prg with
    | Boolean _ -> 0
    | Integer _ -> 0
    | String _ -> 0
    | Unary (_, t) -> max_var' t
    | Binary (_, t1, t2) -> Int.max (max_var' t1) (max_var' t2)
    | If (t1, t2, t3) -> Int.max (Int.max (max_var' t1) (max_var' t2)) (max_var' t3)
    | Abstract (x, t) -> Int.max x (max_var' t)
    | Var x -> x
    | Thunk t -> max_var' !t
  in
  max_var' prg

(** Rename the variable old_var to new_var *)
let var_rename (prg : term) (old_var : int) (new_var : int) : term =
  let rec var_rename' (prg : term) : term =
    match prg with
    | Boolean b -> Boolean b
    | Integer i -> Integer i
    | String s -> String s
    | Unary (op, t) -> Unary (op, var_rename' t)
    | Binary (op, t1, t2) -> Binary (op, var_rename' t1, var_rename' t2)
    | If (t1, t2, t3) -> If (var_rename' t1, var_rename' t2, var_rename' t3)
    | Abstract (x, t) ->
        assert (x <> new_var);
        if x = old_var then Abstract (x, t) else Abstract (x, var_rename' t)
    | Var x -> if x = old_var then Var new_var else Var x
    | Thunk t -> if is_free old_var !t then var_rename' !t else Thunk t
  in

  var_rename' prg

(** Substitute capture-avoidingly the variable for the expr *)
let rec subst (prg : term) (var : int) (expr : term) =
  match prg with
  | Unary (op, t) -> Unary (op, subst t var expr)
  | Binary (op, t1, t2) -> Binary (op, subst t1 var expr, subst t2 var expr)
  | If (t1, t2, t3) -> If (subst t1 var expr, subst t2 var expr, subst t3 var expr)
  | Abstract (x, t) ->
      if x = var || not (is_free var t) then Abstract (x, t)
      else if is_free x expr then
        let fresh_x = 1 + Int.max (max_var t) (max_var expr) in
        let fresh_t = var_rename t x fresh_x in
        Abstract (fresh_x, subst fresh_t var expr)
      else Abstract (x, subst t var expr)
  | Var x -> if x = var then expr else Var x
  | Thunk t -> if is_free var !t then subst !t var expr else Thunk t
  | _ -> prg

let is_value term =
  match term with
  | Boolean _ -> true
  | Integer _ -> true
  | String _ -> true
  | Abstract _ -> true
  | Var _ -> true
  | Unary _ -> false
  | Binary _ -> false
  | If _ -> false
  | Thunk _ -> false

let thunking_enabled = true

let rec eval_step (prg : Language.term) : term =
  match prg with
  | Boolean b -> Boolean b
  | Integer i -> Integer i
  | String s -> String s
  | Abstract (x, t) -> Abstract (x, t)
  | Var x -> Var x
  | Unary (Minus, Integer i) -> Integer (Bigint.neg i)
  | Unary (Not, Boolean b) -> Boolean (not b)
  | Unary (StringToInt, String s) -> Integer (encode_string s |> parse_big_int)
  | Unary (IntToString, Integer i) -> String (deparse_big_int i |> decode_string)
  | Unary (_, t) when is_value t -> failwith "Unexpected unary operand"
  | Unary (op, t) -> Unary (op, eval_step t)
  | Binary (Add, Integer i1, Integer i2) -> Integer (i1 ++ i2)
  | Binary (Sub, Integer i1, Integer i2) -> Integer (i1 -- i2)
  | Binary (Mul, Integer i1, Integer i2) -> Integer (i1 ** i2)
  | Binary (Div, Integer i1, Integer i2) -> Integer (i1 // i2)
  | Binary (Mod, Integer i1, Integer i2) -> Integer (snd (quo_rem i1 i2))
  | Binary (Less, Integer i1, Integer i2) -> Boolean (Bigint.( < ) i1 i2)
  | Binary (Greater, Integer i1, Integer i2) -> Boolean (Bigint.( > ) i1 i2)
  | Binary (Equal, Integer i1, Integer i2) -> Boolean (Bigint.( = ) i1 i2)
  | Binary (Equal, String s1, String s2) -> Boolean (String.equal s1 s2)
  | Binary (Equal, Boolean b1, Boolean b2) -> Boolean (Stdlib.( = ) b1 b2)
  | Binary (And, Boolean b1, Boolean b2) -> Boolean (b1 && b2)
  | Binary (Or, Boolean b1, Boolean b2) -> Boolean (b1 || b2)
  | Binary (StringConcat, String s1, String s2) -> String (s1 ^ s2)
  | Binary (Take, Integer i, String s) -> String (String.prefix s (small i))
  | Binary (Drop, Integer i, String s) -> String (String.drop_prefix s (small i))
  | Binary (Apply, Abstract (x, t), v) ->
      subst t x (if is_value v || not thunking_enabled then v else Thunk (ref v))
  | Binary (Apply, t, v) -> Binary (Apply, eval_step t, v)
  | Binary (_, t1, t2) when is_value t1 && is_value t2 -> failwith "Unexpected binary operand"
  | Binary (op (* not Apply *), t1, t2) -> Binary (op, eval_step t1, eval_step t2)
  | If (Boolean b, t1, t2) -> if b then t1 else t2
  | If (t, _, _) when is_value t -> failwith "Unexpected if operand"
  | If (op, t1, t2) -> If (eval_step op, t1, t2)
  | Thunk t ->
      if is_value !t then !t
      else
        let v = eval_step !t in
        t := v;
        Thunk t

(* evaluates a term to a value directly *)
let rec eval_big_step (prg : Language.term) : term =
  match prg with
  | Boolean b -> Boolean b
  | Integer i -> Integer i
  | String s -> String s
  | Abstract (x, t) -> Abstract (x, t)
  | Var x -> Var x
  | Unary (op, t) -> (
      match (op, eval_big_step t) with
      | Minus, Integer i -> Integer (Bigint.neg i)
      | Not, Boolean b -> Boolean (not b)
      | StringToInt, String s -> Integer (encode_string s |> parse_big_int)
      | IntToString, Integer i -> String (deparse_big_int i |> decode_string)
      | _ -> failwith "Unexpected unary operand")
  | Binary (Apply, t, v) -> (
      match eval_big_step t with
      | Abstract (x, body) -> eval_big_step (subst body x (if is_value v then v else Thunk (ref v)))
      | _ -> failwith "Unexpected Apply operand")
  | Binary (op, t1, t2) -> (
      match (op, eval_big_step t1, eval_big_step t2) with
      | Add, Integer i1, Integer i2 -> Integer (i1 ++ i2)
      | Sub, Integer i1, Integer i2 -> Integer (i1 -- i2)
      | Mul, Integer i1, Integer i2 -> Integer (i1 ** i2)
      | Div, Integer i1, Integer i2 -> Integer (i1 // i2)
      | Mod, Integer i1, Integer i2 -> Integer (snd (quo_rem i1 i2))
      | Less, Integer i1, Integer i2 -> Boolean (Bigint.( < ) i1 i2)
      | Greater, Integer i1, Integer i2 -> Boolean (Bigint.( > ) i1 i2)
      | Equal, Integer i1, Integer i2 -> Boolean (Bigint.( = ) i1 i2)
      | Equal, String s1, String s2 -> Boolean (String.equal s1 s2)
      | Equal, Boolean b1, Boolean b2 -> Boolean (Stdlib.( = ) b1 b2)
      | And, Boolean b1, Boolean b2 -> Boolean (b1 && b2)
      | Or, Boolean b1, Boolean b2 -> Boolean (b1 || b2)
      | StringConcat, String s1, String s2 -> String (s1 ^ s2)
      | Take, Integer i, String s -> String (String.prefix s (small i))
      | Drop, Integer i, String s -> String (String.drop_prefix s (small i))
      | _ -> failwith "Unexpected binary operand")
  | If (cond, t1, t2) -> (
      match eval_big_step cond with
      | Boolean b -> if b then eval_big_step t1 else eval_big_step t2
      | _ -> failwith "Unexpected if operand")
  | Thunk t ->
      if is_value !t then !t
      else
        let v = eval_big_step !t in
        t := v;
        v

let eval (prg : term) : term =
  printf "%s\n%!" (pp_as_lambda 50 prg);
  let rec eval' (prg : term) : term =
    let prg' = eval_step prg in
    if equal_term prg prg' then prg' else eval' prg'
  in
  if thunking_enabled then eval_big_step prg else eval' prg

let%test_unit "eval unop" =
  [%test_result: term] (eval_step (Unary (Minus, Integer (big 1)))) ~expect:(Integer (big (-1)));
  [%test_result: term] (eval_step (Unary (Not, Boolean true))) ~expect:(Boolean false);
  [%test_result: term]
    (eval_step (Unary (StringToInt, String "test")))
    ~expect:(Integer (big 15818151));
  [%test_result: term]
    (eval_step (Unary (IntToString, Integer (big 15818151))))
    ~expect:(String "test")

let%test_unit "eval binop" =
  [%test_result: term]
    (eval_step (Binary (Add, Integer (big 1), Integer (big 2))))
    ~expect:(Integer (big 3));
  [%test_result: term]
    (eval_step (Binary (Sub, Integer (big 1), Integer (big 2))))
    ~expect:(Integer (big (-1)));
  [%test_result: term]
    (eval_step (Binary (Mul, Integer (big 1), Integer (big 2))))
    ~expect:(Integer (big 2));
  [%test_result: term]
    (eval_step (Binary (Div, Integer (big (-7)), Integer (big 2))))
    ~expect:(Integer (big (-3)));
  [%test_result: term]
    (eval_step (Binary (Mod, Integer (big (-7)), Integer (big 2))))
    ~expect:(Integer (big (-1)));
  [%test_result: term]
    (eval_step (Binary (Less, Integer (big 1), Integer (big 2))))
    ~expect:(Boolean true);
  [%test_result: term]
    (eval_step (Binary (Greater, Integer (big 1), Integer (big 2))))
    ~expect:(Boolean false);
  [%test_result: term]
    (eval_step (Binary (Equal, Integer (big 1), Integer (big 2))))
    ~expect:(Boolean false);
  [%test_result: term]
    (eval_step (Binary (And, Boolean true, Boolean false)))
    ~expect:(Boolean false);
  [%test_result: term] (eval_step (Binary (Or, Boolean true, Boolean false))) ~expect:(Boolean true);
  [%test_result: term]
    (eval_step (Binary (StringConcat, String "a", String "b")))
    ~expect:(String "ab");
  [%test_result: term]
    (eval_step (Binary (Take, Integer (big 2), String "abcdef")))
    ~expect:(String "ab");
  [%test_result: term]
    (eval_step (Binary (Drop, Integer (big 2), String "abcdef")))
    ~expect:(String "cdef")

let%test_unit "eval" =
  [%test_result: term] (eval (parse "? B> I# I$ S9%3 S./")) ~expect:(String "no");
  [%test_result: term]
    (eval (parse "B$ B$ L# L$ v# B. SB%,,/ S}Q/2,$_ IK"))
    ~expect:(String "Hello World!");
  [%test_result: term]
    (eval (parse "B$ L# B$ L\" B+ v\" v\" B* I$ I# v8"))
    ~expect:(Integer (parse_big_int "-"));
  [%test_result: term]
    (eval
       (parse
          "B$ B$ L\" B$ L# B$ v\" B$ v# v# L# B$ v\" B$ v# v# L\" L# ? B= v# I! I\" B$ L$ B+ B$ \
           v\" v$ B$ v\" v$ B- v# I\" I%"))
    ~expect:(Integer (big 16));
  [%test_result: term]
    (eval (Binary (Add, Integer (big (-4570084165763281099)), Integer (big 4570084165763281100))))
    ~expect:(Integer (big 1));
  [%test_result: term]
    (eval (Binary (Div, Integer (big 1), Integer (big 4))))
    ~expect:(Integer (big 0))
