(* Add cohttp and cohttp-lwt-unix to your dune file or install them manually *)
(* opam install cohttp cohttp-lwt-unix *)

(* open Lwt.Syntax (* This requires some Dune support *) *)

open Core
open Lwt
open Re

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

let name = Re.compile Re.(seq [ str "["; group (rep1 alnum); str "]" ])
let your_score = Re.compile Re.(seq [ str "Your score: "; group (rep1 digit); str "." ])
let best_score = Re.compile Re.(seq [ str "Best score: "; group (rep1 digit); str "." ])

let parse_scores (s : string) =
  String.split_lines s
  |> List.drop_while ~f:(fun line -> not (String.is_substring line ~substring:"Best score:"))
  |> List.take_while ~f:(fun line -> String.is_substring line ~substring:"Best score:")
  |> List.map ~f:(fun line ->
         let name =
           match Re.exec_opt name line with
           | Some groups -> Group.get groups 1
           | None -> failwith ("Invalid name line: " ^ line)
         in
         let our_score =
           match Re.exec_opt your_score line with
           | Some groups -> Some (Int.of_string (Group.get groups 1))
           | None -> None
         in
         let best_score =
           match Re.exec_opt best_score line with
           | Some groups -> Int.of_string (Group.get groups 1)
           | None -> failwith ("Invalid score line: " ^ line)
         in
         (name, our_score, best_score))

(* Return list of scores for game. Each tuple includes level name, our score (if any) and the best score. *)
let get_scores game =
  let auth_token = Sys.getenv_exn "AUTH_TOKEN" in
  let response = Lwt_main.run (get_body auth_token ("get " ^ game)) in
  match response with
  | String s -> parse_scores s
  | _ -> failwith "Invalid response"
