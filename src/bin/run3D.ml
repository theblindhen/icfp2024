open Core
open Lib

(* Main function *)
let () =
  let input =
    match Sys.get_argv () with
    | [| _; "-" |] -> In_channel.input_all In_channel.stdin
    | [| _; input |] -> input
    | _ -> failwith "Usage: run3D <ASCII_STRING or - for stdin>"
  in
  let grid = ThreeD.parse_grid (String.split_lines input |> Array.of_list) in
  printf "Grid:\n%s" (ThreeD.grid_to_string grid)
