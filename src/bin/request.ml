open Core

(* Add cohttp and cohttp-lwt-unix to your dune file or install them manually *)
(* opam install cohttp cohttp-lwt-unix *)

(* open Lwt.Syntax (* This requires some Dune support *) *)
open Lwt
open Lib

let get_body auth_token input =
  let headers = Cohttp.Header.of_list [ ("Authorization", "Bearer " ^ auth_token) ] in
  let body = Cohttp_lwt.Body.of_string (Language.encode_string_token input) in
  let uri = "https://boundvariable.space/communicate" in
  Cohttp_lwt_unix.Client.post ~headers ~body (Uri.of_string uri) >>= fun (_resp, body) ->
  Cohttp_lwt.Body.to_string body >>= fun body_string ->
  let term = Language.parse body_string in
  (* printf "Received term: %s\n" (Language.sexp_of_term term |> Sexp.to_string_hum); *)
  let evaled = Interpreter.eval term in
  (* printf "Eval: %s" (Language.sexp_of_term evaled |> Sexp.to_string_hum); *)
  Lwt.return evaled

let () =
  let input =
    match Sys.get_argv () with
    | [| _; input |] -> input
    | _ -> failwith "Usage: request ASCII_STRING"
  in
  (* Get auth token from env var AUTH_TOKEN *)
  let auth_token = Sys.getenv_exn "AUTH_TOKEN" in
  let body = Lwt_main.run (get_body auth_token input) in
  match body with
  | String s -> print_endline s
  | Integer i -> print_endline (Int.to_string i)
  | Boolean b -> print_endline (Bool.to_string b)
  | _ -> failwith "Nonliteral eval result"
