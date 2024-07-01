open Core

(* Read a string array type grid from a file *)
let read_grid filename =
  let in_channel = In_channel.create filename in
  let rec aux grid =
    try
      let line = In_channel.input_line_exn in_channel in
      aux (line :: grid)
    with
    | End_of_file ->
        In_channel.close in_channel;
        List.rev grid
  in
  Array.of_list (aux [])

(* Convert a string array grid to a char array array *)
let to_char_array grid = Array.map ~f:String.to_array grid
let big (i : int) = Bigint.of_int i
let small (i : Bigint.t) = Bigint.to_int_exn i
let ( ++ ) = Bigint.( + )
let ( -- ) = Bigint.( - )
let ( ** ) = Bigint.( * )
let ( // ) = Bigint.( / )

(* Load a char array array from a file *)
let load_char_grid filename =
  let input = In_channel.input_all (In_channel.create filename) in
  to_char_array (String.split_lines input |> Array.of_list)

let quo_rem (a : Bigint.t) (b : Bigint.t) =
  let quo = Bigint.( / ) a b in
  let rem = a -- (b ** quo) in
  (quo, rem)

(* Find manhattan distance between two points *)
let manhattan_distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
