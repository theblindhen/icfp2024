open Core
open Lib

let () =
  let game =
    match Sys.get_argv () with
    | [| _; input |] -> input
    | _ -> failwith "Usage: scores game"
  in
  Communication.get_scores game
  |> List.iter ~f:(fun (name, our, best) ->
         match our with
         | Some our -> printf "%s: Our best: %d, Best: %d\n" name our best
         | None -> printf "%s: Best: %d\n" name best)
