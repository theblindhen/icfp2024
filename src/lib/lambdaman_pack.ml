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

let encode_dir dir =
  match dir with
  | 'L' -> 0
  | 'D' -> 1
  | 'U' -> 2
  | 'R' -> 3
  | _ -> failwith "Invalid direction"

let rec encode_dirs dirs =
  if String.length dirs = 0 then 1
  else encode_dir dirs.[0] + (4 * encode_dirs (String.sub dirs 1 (String.length dirs - 1)))

let%test_unit "decode_dirs" =
  [%test_eq: term] (eval (app decode_dirs (Integer (encode_dirs "DUR")))) (String "DUR");
  [%test_eq: term] (eval (app decode_dirs (Integer (encode_dirs "DURL")))) (String "DURL");
  [%test_eq: term] (eval (app decode_dirs (Integer (encode_dirs "DDDDDDDD")))) (String "DDDDDDDD")
