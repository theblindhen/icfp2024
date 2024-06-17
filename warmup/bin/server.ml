open Core
open Opium

let file_handler path _ =
  let index = In_channel.create path |> In_channel.input_all |> String.strip in
  Lwt.return (Response.make ~status:`OK ~body:(Body.of_string index) ())

let hello_handler _ =
  Lwt.return (Response.make ~status:`OK ~body:(Body.of_string "Hello, World!") ())

let _ =
  App.empty
  |> App.get "/" (file_handler "../frontend/index.html")
  |> App.get "/elm.js" (file_handler "../frontend/elm.js")
  |> App.post "/hello" hello_handler
  |> App.run_command
