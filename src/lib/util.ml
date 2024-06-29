open Core

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

(* Convert a grid to a char array array *)
let to_char_array grid = Array.map ~f:String.to_array grid
