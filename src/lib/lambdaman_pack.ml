open Language
open Metalanguage
open Interpreter

let decode_dir =
  abs (fun dir ->
      if_op (eq_op dir (Integer 0)) (String "L")
        (if_op (eq_op dir (Integer 1)) (String "D")
           (if_op (eq_op dir (Integer 2)) (String "U") (String "R"))))

let rec_body =
  abs (fun rec_f ->
      abs (fun arg ->
          if_op (eq_op arg (Integer 1)) (String "")
            (let_op (mod_op arg (Integer 4)) (fun i ->
                 let_op (div_op arg (Integer 4)) (fun r ->
                     let_op (app decode_dir i) (fun c -> concat_op c (app rec_f r)))))))

(** ICFP term for decoding integers to L/D/U/R efficiently *)
let decode_dirs = app rec_op rec_body

let%test_unit "decode_dirs" =
  [%test_eq: term] (eval (app decode_dirs (Integer 121))) (String "DUR");
  [%test_eq: term] (eval (app decode_dirs (Integer 313))) (String "DURL")
