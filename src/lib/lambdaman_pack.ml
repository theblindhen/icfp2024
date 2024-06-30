open Core
open Language
open Metalanguage
open Interpreter
open Util

let decode_dir dir = take_op (Integer (big 1)) (drop_op dir (String "LDUR"))

let decode_dirs_body =
  abs (fun rec_f ->
      abs (fun arg ->
          if_op
            (eq_op arg (Integer (big 1)))
            (String "")
            (concat_op
               (decode_dir (mod_op arg (Integer (big 4))))
               (app rec_f (div_op arg (Integer (big 4)))))))

(** May be called on any integer but is incapable of returning trailing Ls *)
let decode_dirs_body_safe =
  abs (fun rec_f ->
      abs (fun arg ->
          if_op
            (eq_op arg (Integer (big 0)))
            (String "")
            (concat_op
               (decode_dir (mod_op arg (Integer (big 4))))
               (app rec_f (div_op arg (Integer (big 4)))))))

(** ICFP term for decoding integers to L/D/U/R efficiently *)
let decode_dirs = app rec_op decode_dirs_body

let decode_dirs_safe = app rec_op decode_dirs_body_safe

let encode_dir dir =
  match dir with
  | 'L' -> big 0
  | 'D' -> big 1
  | 'U' -> big 2
  | 'R' -> big 3
  | _ -> failwith "Invalid direction"

let rec encode_dirs dirs =
  if String.length dirs = 0 then big 1
  else
    encode_dir dirs.[0]
    ++ (big 4 ** encode_dirs (String.sub dirs ~pos:1 ~len:(String.length dirs - 1)))

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

let repeat_r_body =
  abs (fun rec_f ->
      abs (fun n ->
          if_op
            (eq_op n (Integer (big 0)))
            (String "")
            (concat_op (String "R") (app rec_f (sub_op n (Integer (big 1)))))))

(* ICFP term for repeating a string n times *)
let repeat_r = app rec_op repeat_r_body

exception Rep of int * int

let find_good_rep dirs =
  if String.length dirs = 0 then None
  else
    let curr_char = ref dirs.[0] in
    let curr_len = ref 1 in
    try
      for i = 1 to String.length dirs - 1 do
        if Char.equal dirs.[i] !curr_char then incr curr_len
        else if !curr_len >= 40 then raise (Rep (i - !curr_len, !curr_len))
        else (
          curr_char := dirs.[i];
          curr_len := 1)
      done;
      None
    with
    | Rep (i, len) -> Some (i, len)

let encode_as_repeats dirs =
  let rec encode' repeat decode_dirs dirs =
    if String.length dirs = 0 then String ""
    else
      match find_good_rep dirs with
      | Some (0, len) ->
          concat_op
            (app (app repeat (String (String.of_char dirs.[0]))) (Integer (big len)))
            (encode' repeat decode_dirs (String.drop_prefix dirs len))
      | Some (i, _) ->
          let prefix = app decode_dirs (Integer (encode_dirs (String.prefix dirs i))) in
          concat_op prefix (encode' repeat decode_dirs (String.drop_prefix dirs i))
      | None -> app decode_dirs (Integer (encode_dirs dirs))
  in
  let_op repeat (fun repeat_f ->
      let_op decode_dirs (fun decode_dirs -> encode' repeat_f decode_dirs dirs))

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

let%test_unit "repeat" =
  [%test_eq: term] (eval (app (app repeat (String "a")) (Integer (big 3)))) (String "aaa");
  [%test_eq: term] (eval (app (app repeat (String "ab")) (Integer (big 3)))) (String "ababab");
  [%test_eq: term] (eval (app (app repeat (String "ab")) (Integer (big 0)))) (String "")

let%test_unit "encode_as_repeats" =
  [%test_eq: term]
    (eval (encode_as_repeats "DDDDDDDDDUUUUUUUUUUUUU"))
    (String "DDDDDDDDDUUUUUUUUUUUUU")
