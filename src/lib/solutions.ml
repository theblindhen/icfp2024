open Core

let solutions_dir game map_dir level = map_dir ^ "/" ^ game ^ level

let solution_file ?suffix game map_dir level score =
  solutions_dir game map_dir level
  ^ "/"
  ^ Int.to_string score
  ^ Option.value_map ~default:"" ~f:(fun s -> "." ^ s) suffix

let score sol = String.length sol

let create_solutions_dir game map_dir level =
  match Sys_unix.file_exists (solutions_dir game map_dir level) with
  | `No -> Core_unix.mkdir (solutions_dir game map_dir level)
  | _ -> ()

let write_solution ?suffix ?alt_score game map_dir level sol =
  create_solutions_dir game map_dir level;
  let score = Option.value alt_score ~default:(score sol) in
  let solution_filename = solution_file ?suffix game map_dir level score in
  match Sys_unix.file_exists solution_filename with
  | `No -> Out_channel.write_all solution_filename ~data:sol
  | _ -> ()

let best_sol dir name =
  let sol_dir = dir ^ "/" ^ name in
  match Sys_unix.file_exists sol_dir with
  | `Yes -> (
      let files = Sys_unix.ls_dir sol_dir in
      let best =
        files |> List.filter_map ~f:Int.of_string_opt |> List.min_elt ~compare:Int.compare
      in
      match best with
      | Some best ->
          let best_file = sol_dir ^ "/" ^ Int.to_string best in
          let sol = In_channel.read_all best_file in
          Some (best, best_file, sol)
      | None -> None)
  | _ -> None
