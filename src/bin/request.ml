open Core
open Lib

let () =
  let input =
    match Sys.get_argv () with
    | [| _; input |] -> input
    | _ -> failwith "Usage: request ASCII_STRING"
  in
  (* Get auth token from env var AUTH_TOKEN *)
  let auth_token = Sys.getenv_exn "AUTH_TOKEN" in
  let body = Lwt_main.run (Communication.get_body auth_token input) in
  match body with
  | String s -> print_endline s
  | Integer i -> print_endline (Int.to_string i)
  | Boolean b -> print_endline (Bool.to_string b)
  | _ -> failwith "Nonliteral eval result"
