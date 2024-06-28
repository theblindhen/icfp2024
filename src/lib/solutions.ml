open Core

let solutions_dir map_dir level = map_dir ^ "/spaceship" ^ level
let solution_file map_dir level score = solutions_dir map_dir level ^ "/" ^ Int.to_string score
let score sol = List.length sol

let create_solutions_dir map_dir level =
  match Sys_unix.file_exists (solutions_dir map_dir level) with
  | `No -> Core_unix.mkdir (solutions_dir map_dir level)
  | _ -> ()

let write_solution map_dir level sol =
  create_solutions_dir map_dir level;
  let score = score sol in
  let solution_filename = solution_file map_dir level score in
  match Sys_unix.file_exists solution_filename with
  | `No -> Out_channel.write_all (solution_file map_dir level score) ~data:(String.of_char_list sol)
  | _ -> ()
