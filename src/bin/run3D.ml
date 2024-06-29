open Core
open Lib

exception FinishedExc of Bigint.t option

(* Main function *)
let () =
  let iters, a, b, input =
    let get_input = function
      | "-" -> In_channel.input_all In_channel.stdin
      | input -> input
    in
    match Sys.get_argv () |> List.of_array with
    | [ _; "-i"; i; a; b; input_ch ] ->
        (Int.of_string i, Bigint.of_string a, Bigint.of_string b, get_input input_ch)
    | [ _; a; b; input_ch ] -> (5, Bigint.of_string a, Bigint.of_string b, get_input input_ch)
    | _ -> failwith "Usage: run3D A B <ASCII_STRING or - for stdin>"
  in
  let state = ref (ThreeD.init_state input a b) in
  printf "Initial state:\n%s" (ThreeD.dump_state !state);
  try
    for _ = 1 to iters do
      state := ThreeD.step !state;
      printf "\n\n%s" (ThreeD.dump_state !state);
      if Stdlib.( <> ) !state.return_value None then raise (FinishedExc !state.return_value) else ()
    done
  with
  | FinishedExc _ret -> printf "Done\n"
