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
let eq_op t1 t2 = Binary (Equal, t1, t2)
let take_op t1 t2 = Binary (Take, t1, t2)
let drop_op t1 t2 = Binary (Drop, t1, t2)
let mod_op t1 t2 = Binary (Mod, t1, t2)
let div_op t1 t2 = Binary (Div, t1, t2)
let concat_op t1 t2 = Binary (StringConcat, t1, t2)
let sToI t = Unary (StringToInt, t)

(* ICFP term recursion operator *)
(* forall alpha. (alpha -> alpha) -> alpha *)
let rec_op = abs (fun f -> app (abs (fun x -> app f (app x x))) (abs (fun x -> app f (app x x))))
