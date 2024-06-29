open Core
open Lib

let () =
  let is_prg, input =
    match Sys.get_argv () with
    | [| _; "-" |] -> (false, In_channel.input_all In_channel.stdin)
    | [| _; input |] -> (false, input)
    | [| _; "-p"; "-" |] -> (true, In_channel.input_all In_channel.stdin)
    | [| _; "-p"; input |] -> (true, input)
    | _ -> failwith "Usage: request [-p] <ASCII_STRING or - for stdin>"
  in
  let token =
    if is_prg then
      String.substr_replace_all input ~pattern:"\n" ~with_:" "
      |> Sexp.of_string
      |> Language.term_of_sexp
      |> Language.deparse
    else Language.encode_string_token input
  in
  let response = Communication.request_with_auth token in
  match response with
  | String s -> print_endline s
  | Integer i -> print_endline (Int.to_string i)
  | Boolean b -> print_endline (Bool.to_string b)
  | _ -> failwith "Nonliteral eval result"
