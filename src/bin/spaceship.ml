open Core

(*
From the task description:

The game operates on an infinite 2D chess board, with the spaceship initially
located on (0,0). The spaceship has a velocity vx and vy, which are initially
both set to 0. In each turn the player can increase/decrease each of those
numbers by at most one, and then the piece moves vx steps to the right and vy
steps up.

Moves are represented with a single digit, inspired by the old numeric pad on a
computer keyboard that we used to have in the old days on Earth. For example, 7
means decreasing vx and increasing vy by 1, while 6 means increasing vx by 1
and keeping vy the same. A path can then be represented by a sequence of
digits, e.g. the path 236659 visits, in this order, the following squares:
  (0,0) (0,-1) (1,-3) (3,-5) (6,-7) (9,-9) (13,-10).

Now the challenge is the following: given a list of squares to be visited, find
a sequence of moves that visits all those squares.
 *)

(* Numeric keypad, for reference:
    7 8 9
    4 5 6
    1 2 3 *)
let move_of_direction (ax, ay) =
  match (ax, ay) with
  | -1, -1 -> '1'
  | 0, -1 -> '2'
  | 1, -1 -> '3'
  | -1, 0 -> '4'
  | 0, 0 -> '5'
  | 1, 0 -> '6'
  | -1, 1 -> '7'
  | 0, 1 -> '8'
  | 1, 1 -> '9'
  | _ -> failwith "Can't move this far in one step"

let () =
  let map_file =
    match Sys.get_argv () with
    | [| _; filename |] -> filename
    | _ -> failwith "Usage: spaceship MAP_FILE"
  in
  let problem =
    In_channel.read_lines map_file
    |> List.filter ~f:(String.( <> ) "") (* Skip empty lines *)
    |> List.map ~f:(fun line ->
           match String.split line ~on:' ' with
           | [ key; value ] -> (Int.of_string key, Int.of_string value)
           | _ -> failwith ("Invalid line: " ^ line))
  in
  (* Simplest possible solution strategy:
      - While where are squares to visit:
        - Pick the next square (later: the closest square)
        - Move towards it with both vx and vy between -1 and 1 (later: larger values)
        - Stop when we reach it (later: keep moving)
  *)
  let sol =
    let rec step_to_point (distx, disty) (vx, vy) =
      (*let () = Printf.eprintf "dist: (%d, %d), vel: (%d, %d)\n" distx disty vx vy in*)
      if distx = 0 && disty = 0 && vx = 0 && vy = 0 then []
      else
        let vx' = Sign.to_int (Int.sign distx) in
        let vy' = Sign.to_int (Int.sign disty) in
        let ax = vx' - vx in
        let ay = vy' - vy in
        move_of_direction (ax, ay) :: step_to_point (distx - vx', disty - vy') (vx', vy')
    in
    let rec to_remaining_points (startx, starty) points =
      match points with
      | [] -> []
      | (endx, endy) :: points ->
          let distx = endx - startx in
          let disty = endy - starty in
          let steps = step_to_point (distx, disty) (0, 0) in
          steps :: to_remaining_points (endx, endy) points
    in
    List.concat (to_remaining_points (0, 0) problem)
  in
  print_endline (String.of_char_list sol);
  Printf.eprintf "Length: %d\n" (List.length sol);
  ()
