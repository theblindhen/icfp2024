open Core

(* Add cohttp and cohttp-lwt-unix to your dune file or install them manually *)
(* opam install cohttp cohttp-lwt-unix *)

(* open Lwt.Syntax (* This requires some Dune support *) *)
open Lwt
open Cohttp
open Cohttp_lwt_unix
open Lib

let get_body auth_token uri =
  let headers = Header.of_list [ ("Authorization", "Bearer " ^ auth_token) ] in
  let body = Cohttp_lwt.Body.of_string "S'%4}).$%8" in
  Client.post ~headers ~body (Uri.of_string uri) >>= fun (_resp, body) ->
  Cohttp_lwt.Body.to_string body >>= fun body_string ->
  Lwt.return (Language.decode_token body_string)

let () =
  (* Get auth token from env var AUTH_TOKEN *)
  let auth_token = Sys.getenv_exn "AUTH_TOKEN" in
  let uri = "https://boundvariable.space/communicate" in
  let body = Lwt_main.run (get_body auth_token uri) in
  print_endline body
