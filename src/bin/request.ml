open Core
open Lib

let command =
  Command.basic ~summary:"Post a request to the server"
    (let%map_open.Command is_prg = flag "-p" no_arg ~doc:"Take input as S-expression"
     and no_eval = flag "-E" no_arg ~doc:"Don't evaluate the response"
     and input = anon ("INPUT" %: string) in
     fun () ->
       let input =
         match input with
         | "-" -> In_channel.input_all In_channel.stdin
         | input -> input
       in
       let token =
         if is_prg then
           String.substr_replace_all input ~pattern:"\n" ~with_:" "
           |> Sexp.of_string
           |> Language.term_of_sexp
           |> Language.deparse
         else Language.encode_string_token input
       in
       let response = Communication.request_unevaled_with_auth token in
       let response = if no_eval then response else Interpreter.eval_big_step response in
       match response with
       | String s -> print_endline s
       | Integer i -> print_endline (Bigint.to_string_hum i)
       | Boolean b -> print_endline (Bool.to_string b)
       | _ -> print_endline (Language.pp_as_lambda 50 response))

let () = Command_unix.run command
