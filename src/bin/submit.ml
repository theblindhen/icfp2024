open Core
open Lib

let best_sol dir name =
  let sol_dir = dir ^ "/" ^ name in
  match Sys_unix.file_exists sol_dir with
  | `Yes -> (
      let files = Sys_unix.ls_dir sol_dir in
      let best = files |> List.map ~f:Int.of_string |> List.min_elt ~compare:Int.compare in
      match best with
      | Some best ->
          let best_file = sol_dir ^ "/" ^ Int.to_string best in
          let sol = In_channel.read_all best_file in
          Some (best, best_file, sol)
      | None -> None)
  | _ -> None

let () =
  let game, dir =
    match Sys.get_argv () with
    | [| _; game; dir |] -> (game, dir)
    | _ -> failwith "Usage: submit game dir"
  in
  Communication.get_scores game
  |> List.iter ~f:(fun (name, our, best) ->
         match best_sol dir name with
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
