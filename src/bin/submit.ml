open Core
open Lib

let () =
  let game, dir =
    match Sys.get_argv () with
    | [| _; game; dir |] -> (game, dir)
    | _ -> failwith "Usage: submit game dir"
  in
  let submit =
    match game with
    | "spaceship" -> fun name sol -> Communication.submit_solution name sol
    | "lambdaman" -> (
        fun _ sol ->
          match Communication.request_with_auth sol with
          | Language.String s -> print_endline s
          | _ -> failwith "Invalid response")
    | _ -> failwith ("Unknown game: " ^ game)
  in
  Communication.get_scores game
  |> List.iter ~f:(fun (name, our, best) ->
         match Solutions.best_sol dir name with
         | Some (best_local_score, best_local_file, sol) ->
             let prev_desc, local_better =
               match our with
               | Some our ->
                   ("our previous score was: " ^ Int.to_string our ^ "; ", best_local_score < our)
               | None -> ("this is our first solution; ", true)
             in
             if local_better then (
               printf "Submitting solution for %s with score %d (%sbest score is %d)\n" name
                 best_local_score prev_desc best;
               submit name sol;
               Core_unix.fork_exec ~prog:"git" ~argv:[ "git"; "add"; best_local_file ] () |> ignore)
         | None -> ())
