open Core

(* Add cohttp and cohttp-lwt-unix to your dune file or install them manually *)
(* opam install cohttp cohttp-lwt-unix *)

(* open Lwt.Syntax (* This requires some Dune support *) *)
open Lwt
open Lib

let get_body auth_token input =
  let headers = Cohttp.Header.of_list [ ("Authorization", "Bearer " ^ auth_token) ] in
  let body = Cohttp_lwt.Body.of_string (Language.encode_string input) in
  let uri = "https://boundvariable.space/communicate" in
  Cohttp_lwt_unix.Client.post ~headers ~body (Uri.of_string uri) >>= fun (_resp, body) ->
  Cohttp_lwt.Body.to_string body >>= fun body_string ->
  Lwt.return (Language.decode_token body_string)

let () =
  let input =
    match Sys.get_argv () with
    | [| _; input |] -> input
    | _ -> failwith "Usage: request ASCII_STRING"
  in
  (* Get auth token from env var AUTH_TOKEN *)
  let auth_token = Sys.getenv_exn "AUTH_TOKEN" in
  let body = Lwt_main.run (get_body auth_token input) in
  print_endline body
