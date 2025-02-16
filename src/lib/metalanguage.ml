open Language

(** Generic ICFP expression operators *)

let vars = ref 1

let abs body =
  let var = !vars in
  vars := !vars + 1;
  Abstract (var, body (Var var))

let app t1 t2 = Binary (Apply, t1, t2)

(* let operator *)
let let_op expr body = app (abs body) expr
let if_op cond t e = If (cond, t, e)
let add_op t1 t2 = Binary (Add, t1, t2)
let mult_op t1 t2 = Binary (Mul, t1, t2)
let eq_op t1 t2 = Binary (Equal, t1, t2)
let take_op t1 t2 = Binary (Take, t1, t2)
let drop_op t1 t2 = Binary (Drop, t1, t2)
let lt_op t1 t2 = Binary (Less, t1, t2)
let mod_op t1 t2 = Binary (Mod, t1, t2)
let div_op t1 t2 = Binary (Div, t1, t2)
let sub_op t1 t2 = Binary (Sub, t1, t2)
let concat_op t1 t2 = Binary (StringConcat, t1, t2)
let sToI t = Unary (StringToInt, t)
let iToS t = Unary (IntToString, t)

(* ICFP term recursion operator *)
(* forall alpha. (alpha -> alpha) -> alpha *)
let rec_op = abs (fun f -> let_op (abs (fun x -> app f (app x x))) (fun x -> app x x))
