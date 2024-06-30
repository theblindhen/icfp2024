open Core
open Lib
open Metalanguage

let wrap_prefix level term = concat_op (String ("solve lambdaman" ^ Int.to_string level ^ " ")) term

let hand_solutions =
  [|
    [||];
    [||];
    [||];
    [||];
    [||];
    [|
      wrap_prefix 6
        (let_op
           (abs (fun a -> concat_op a (concat_op (concat_op a a) (concat_op a a))))
           (fun mult -> app mult (app mult (String "RRRRRRRR"))));
      wrap_prefix 6
        (let_op
           (abs (fun a -> concat_op a a))
           (fun double ->
             let_op (String "RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR") (fun x ->
                 app double (app double x))));
      wrap_prefix 6
        (let_op
           (abs (fun a -> concat_op a (concat_op a a)))
           (fun double ->
             drop_op
               (Integer (Util.big 16))
               (app double (app double (app double (String "RRRRRRRR"))))));
      wrap_prefix 6
        (let_op
           (abs (fun a -> concat_op a (concat_op (concat_op a a) (concat_op a a))))
           (fun mult -> app mult (app mult (String "RRRRRRRR"))));
    |];
  |]

(* Main function *)
let () =
  if Array.length (Sys.get_argv ()) <> 3 then
    printf "Usage: %s <input_dir> <level>\n" (Sys.get_argv ()).(0)
  else
    let dir = (Sys.get_argv ()).(1) in
    let level = Int.of_string (Sys.get_argv ()).(2) in
    if level - 1 < Array.length hand_solutions then
      let sols = Array.get hand_solutions (level - 1) in
      Array.iter sols ~f:(fun sol ->
          let icfp = Language.deparse sol in
          printf "%5d: %s\n" (String.length icfp) icfp;
          Solutions.write_solution "lambdaman" dir (Int.to_string level) icfp)
    else printf "No handwritten solution for level %d\n" level
