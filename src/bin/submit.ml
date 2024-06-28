open Core
open Lib

let () =
  let game, dir =
    match Sys.get_argv () with
    | [| _; game; dir |] -> (game, dir)
    | _ -> failwith "Usage: submit game dir"
  in
  Communication.get_scores game
  |> List.iter ~f:(fun (name, our, best) ->
         match Solutions.best_sol dir name with
         | Some (best_local_score, best_local_file, sol) ->
             let local_better =
               match our with
               | Some our -> best_local_score < our
               | None -> true
             in
             if local_better then (
               printf "Submitting solution for %s with score %d (best score is %d)\n" name
                 best_local_score best;
               Communication.submit_solution name sol;
               Core_unix.fork_exec ~prog:"git" ~argv:[ "git"; "add"; best_local_file ] () |> ignore)
         | None -> ())
