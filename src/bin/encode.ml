open Core
open Lib

let () =
  (* Read stdin as a string into a varible *)
  let input = In_channel.input_all In_channel.stdin in
  (* Trim whitespace from input *)
  let input = String.strip input in
  let output = Language.encode_string_token input in
  Printf.printf "%s\n" output
