open Core
open Util

(** Completely random moves *)
let random_moves n = String.init n ~f:(fun _ -> "RULD".[Random.int (String.length "RULD")])

(** Repeat the input N times *)
let repeat s n = String.concat (List.init n ~f:(fun _ -> s))

let repeated_random randoms total =
  let n = (total / randoms) + 1 in
  repeat (random_moves randoms) n
