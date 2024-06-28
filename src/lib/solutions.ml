open Core

let solutions_dir game map_dir level = map_dir ^ "/" ^ game ^ level

let solution_file game map_dir level score =
  solutions_dir game map_dir level ^ "/" ^ Int.to_string score

let score sol = String.length sol

let create_solutions_dir game map_dir level =
  match Sys_unix.file_exists (solutions_dir game map_dir level) with
  | `No -> Core_unix.mkdir (solutions_dir game map_dir level)
  | _ -> ()

let write_solution game map_dir level sol =
  create_solutions_dir game map_dir level;
  let score = score sol in
  let solution_filename = solution_file game map_dir level score in
  match Sys_unix.file_exists solution_filename with
  | `No -> Out_channel.write_all solution_filename ~data:sol
  | _ -> ()

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
