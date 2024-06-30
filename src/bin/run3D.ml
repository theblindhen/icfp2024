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
  let grid_input =
    let lines = String.split input ~on:'\n' in
    let first_line = lines |> List.hd_exn in
    if Stdlib.( = ) (String.prefix first_line 5) "solve" then
      List.tl_exn lines |> String.concat ~sep:"\n"
    else input
  in
  let state = ref (ThreeD.init_state grid_input a b) in
  printf "Initial state:\n%s" (ThreeD.dump_state !state);
  (try
     for _ = 1 to iters do
       state := ThreeD.step !state;
       printf "\n\n%s" (ThreeD.dump_state !state);
       if Stdlib.( <> ) !state.return_value None then raise (FinishedExc !state.return_value)
       else if not !state.is_changing then raise (FinishedExc None)
       else ()
     done
   with
  | FinishedExc None -> printf "Finished without return value\n"
  | FinishedExc _ -> printf "DONE\n");
  printf "Simulation cost: %d\n" (ThreeD.cost !state)
