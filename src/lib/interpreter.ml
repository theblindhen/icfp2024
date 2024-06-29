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
  | _ -> prg

let rec eval_step (prg : Language.term) : term =
  match prg with
  | Boolean b -> Boolean b
  | Integer i -> Integer i
  | String s -> String s
  | Abstract (x, t) -> Abstract (x, t)
  | Var x -> Var x
  | Unary (Minus, Integer i) -> Integer (-i)
  | Unary (Not, Boolean b) -> Boolean (not b)
  | Unary (StringToInt, String s) -> Integer (encode_string s |> parse_int)
  | Unary (IntToString, Integer i) -> String (deparse_int i |> decode_string)
  | Unary (op, t) -> Unary (op, eval_step t)
  | Binary (Add, Integer i1, Integer i2) -> Integer (i1 + i2)
  | Binary (Sub, Integer i1, Integer i2) -> Integer (i1 - i2)
  | Binary (Mul, Integer i1, Integer i2) -> Integer (i1 * i2)
  | Binary (Div, Integer i1, Integer i2) -> Integer (i1 / i2)
  | Binary (Mod, Integer i1, Integer i2) -> Integer (Int.rem i1 i2)
  | Binary (Less, Integer i1, Integer i2) -> Boolean (i1 < i2)
  | Binary (Greater, Integer i1, Integer i2) -> Boolean (i1 > i2)
  | Binary (Equal, Integer i1, Integer i2) -> Boolean (i1 = i2)
  | Binary (Equal, String s1, String s2) -> Boolean (String.equal s1 s2)
  | Binary (Equal, Boolean b1, Boolean b2) -> Boolean (Stdlib.( = ) b1 b2)
  | Binary (And, Boolean b1, Boolean b2) -> Boolean (b1 && b2)
  | Binary (Or, Boolean b1, Boolean b2) -> Boolean (b1 || b2)
  | Binary (StringConcat, String s1, String s2) -> String (s1 ^ s2)
  | Binary (Take, Integer i, String s) -> String (String.prefix s i)
  | Binary (Drop, Integer i, String s) -> String (String.drop_prefix s i)
  | Binary (Apply, Abstract (x, t), v) -> subst t x v
  | Binary (Apply, t, v) -> Binary (Apply, eval_step t, v)
  | Binary (op (* not Apply *), t1, t2) -> Binary (op, eval_step t1, eval_step t2)
  | If (Boolean b, t1, t2) -> if b then t1 else t2
  | If (op, t1, t2) -> If (eval_step op, t1, t2)

let eval (prg : term) : term =
  let rec eval' (prg : term) : term =
    let prg' = eval_step prg in
    if equal_term prg' prg then prg else eval' prg'
  in
  eval' prg

let%test_unit "eval unop" =
  [%test_result: term] (eval_step (Unary (Minus, Integer 1))) ~expect:(Integer (-1));
  [%test_result: term] (eval_step (Unary (Not, Boolean true))) ~expect:(Boolean false);
  [%test_result: term] (eval_step (Unary (StringToInt, String "test"))) ~expect:(Integer 15818151);
  [%test_result: term] (eval_step (Unary (IntToString, Integer 15818151))) ~expect:(String "test")

let%test_unit "eval binop" =
  [%test_result: term] (eval_step (Binary (Add, Integer 1, Integer 2))) ~expect:(Integer 3);
  [%test_result: term] (eval_step (Binary (Sub, Integer 1, Integer 2))) ~expect:(Integer (-1));
  [%test_result: term] (eval_step (Binary (Mul, Integer 1, Integer 2))) ~expect:(Integer 2);
  [%test_result: term] (eval_step (Binary (Div, Integer (-7), Integer 2))) ~expect:(Integer (-3));
  [%test_result: term] (eval_step (Binary (Mod, Integer (-7), Integer 2))) ~expect:(Integer (-1));
  [%test_result: term] (eval_step (Binary (Less, Integer 1, Integer 2))) ~expect:(Boolean true);
  [%test_result: term] (eval_step (Binary (Greater, Integer 1, Integer 2))) ~expect:(Boolean false);
  [%test_result: term] (eval_step (Binary (Equal, Integer 1, Integer 2))) ~expect:(Boolean false);
  [%test_result: term]
    (eval_step (Binary (And, Boolean true, Boolean false)))
    ~expect:(Boolean false);
  [%test_result: term] (eval_step (Binary (Or, Boolean true, Boolean false))) ~expect:(Boolean true);
  [%test_result: term]
    (eval_step (Binary (StringConcat, String "a", String "b")))
    ~expect:(String "ab");
  [%test_result: term] (eval_step (Binary (Take, Integer 2, String "abcdef"))) ~expect:(String "ab");
  [%test_result: term]
    (eval_step (Binary (Drop, Integer 2, String "abcdef")))
    ~expect:(String "cdef")

let%test_unit "eval" =
  [%test_result: term] (eval (parse "? B> I# I$ S9%3 S./")) ~expect:(String "no");
  [%test_result: term]
    (eval (parse "B$ B$ L# L$ v# B. SB%,,/ S}Q/2,$_ IK"))
    ~expect:(String "Hello World!");
  [%test_result: term]
    (eval (parse "B$ L# B$ L\" B+ v\" v\" B* I$ I# v8"))
    ~expect:(Integer (parse_int "-"));
  [%test_result: term]
    (eval
       (parse
          "B$ B$ L\" B$ L# B$ v\" B$ v# v# L# B$ v\" B$ v# v# L\" L# ? B= v# I! I\" B$ L$ B+ B$ \
           v\" v$ B$ v\" v$ B- v# I\" I%"))
    ~expect:(Integer 16)
