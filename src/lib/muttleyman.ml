open Core
open Util

let encode_dir = function
  | 'R' -> big 0
  | 'L' -> big 1
  | 'U' -> big 2
  | 'D' -> big 3
  | _ -> failwith "Invalid move"

let decode_dir = function
  | 0 -> 'R'
  | 1 -> 'L'
  | 2 -> 'U'
  | 3 -> 'D'
  | _ -> failwith "Invalid move"

let encode_dirs s =
  String.to_list s
  |> List.map ~f:encode_dir
  |> List.fold ~init:Bigint.zero ~f:(fun acc x -> Bigint.((acc * big 4) + x))

let decode_dirs n =
  let rec aux acc n =
    if Bigint.(n = zero) then acc
    else
      let q, r = quo_rem n (big 4) in
      aux (decode_dir (small r) :: acc) q
  in
  aux [] n |> String.of_char_list

(** Completely random moves *)
let random_moves n = String.init n ~f:(fun _ -> "RULD".[Random.int (String.length "RULD")])

(** Repeat the input N times *)
let repeat s n = String.concat (List.init n ~f:(fun _ -> s))

let repeat_random seed_len total =
  let n = (total / seed_len) + 1 in
  repeat (random_moves seed_len) n

let pseudo_repeat_random seed_len total =
  let n = (total / seed_len) + 1 in
  let seed_str = random_moves seed_len in
  let seed = encode_dirs seed_str in
  let rec aux acc i =
    if i = n then acc
    else
      let shuffle_seed = Bigint.(seed * (seed + big i)) in
      aux (decode_dirs shuffle_seed :: acc) (i + 1)
  in
  aux [] 0 |> String.concat
