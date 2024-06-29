open Language
open Metalanguage
open Interpreter

let decode_dir =
  abs (fun dir ->
      if_op
        (eq_op dir (Integer (big 0)))
        (String "L")
        (if_op
           (eq_op dir (Integer (big 1)))
           (String "D")
           (if_op (eq_op dir (Integer (big 2))) (String "U") (String "R"))))

let decode_dirs_body =
  abs (fun rec_f ->
      abs (fun arg ->
          if_op
            (eq_op arg (Integer (big 1)))
            (String "")
            (let_op
               (mod_op arg (Integer (big 4)))
               (fun i ->
                 let_op
                   (div_op arg (Integer (big 4)))
                   (fun r -> let_op (app decode_dir i) (fun c -> concat_op c (app rec_f r)))))))

(** ICFP term for decoding integers to L/D/U/R efficiently *)
let decode_dirs = app rec_op decode_dirs_body

let repeat_body =
  abs (fun rec_f ->
      abs (fun str ->
          abs (fun n ->
              if_op
                (eq_op n (Integer (big 0)))
                (String "")
                (concat_op str (app (app rec_f str) (sub_op n (Integer (big 1))))))))

(* ICFP term for repeating a string n times *)
let repeat = app rec_op repeat_body

let encode_dir dir =
  match dir with
  | 'L' -> big 0
  | 'D' -> big 1
  | 'U' -> big 2
  | 'R' -> big 3
  | _ -> failwith "Invalid direction"

let rec encode_dirs dirs =
  if String.length dirs = 0 then big 1
  else encode_dir dirs.[0] ++ (big 4 ** encode_dirs (String.sub dirs 1 (String.length dirs - 1)))

let%test_unit "decode_dirs" =
  [%test_eq: term] (eval (app decode_dirs (Integer (encode_dirs "DUR")))) (String "DUR");
  [%test_eq: term] (eval (app decode_dirs (Integer (encode_dirs "DURL")))) (String "DURL");
  [%test_eq: term] (eval (app decode_dirs (Integer (encode_dirs "DDDDDDDD")))) (String "DDDDDDDD");
  [%test_eq: term]
    (eval (app decode_dirs (Integer (encode_dirs "DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD"))))
    (String "DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD")

let%test_unit "repeat" =
  [%test_eq: term] (eval (app (app repeat (String "a")) (Integer (big 3)))) (String "aaa");
  [%test_eq: term] (eval (app (app repeat (String "ab")) (Integer (big 3)))) (String "ababab");
  [%test_eq: term] (eval (app (app repeat (String "ab")) (Integer (big 0)))) (String "")
