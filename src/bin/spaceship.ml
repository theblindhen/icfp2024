open Core
open Lib

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

let direction_of_move = function
  | '1' -> (-1, -1)
  | '2' -> (0, -1)
  | '3' -> (1, -1)
  | '4' -> (-1, 0)
  | '5' -> (0, 0)
  | '6' -> (1, 0)
  | '7' -> (-1, 1)
  | '8' -> (0, 1)
  | '9' -> (1, 1)
  | _ -> failwith "No such move exists"

(* Gets the square closest to a given point, measured by chessboard distance. *)
let closest_square (x, y) squares =
  List.map squares ~f:(fun ((x', y') as xy') -> (xy', max (abs (x' - x)) (abs (y' - y))))
  |> List.min_elt ~compare:(fun (_, d1) (_, d2) -> Int.compare d1 d2)
  |> Option.map ~f:fst

let solve (problem : (int * int) list) =
  (* Solution strategy:
      - While where are squares to visit:
        - Pick the closest square
        - Move towards it with both vx and vy between -1 and 1 (later: larger values)
        - Stop when we reach it (later: keep moving)
  *)
  let vmax dist =
    (*
      How much can we speed up when there's distance d left, assuming we need
      to slow down to a standstill before we get there?

      Let's say we're considering whether to move at speed 3. Then we must go
      3+2+1=6 steps before we're stopped. Taking that backwards, if we're at distance 6
      from the target, we can speed up to 3.
      Generally, d >= v + (v-1) + ... + 0 = v*(v+1)/2, so v <= sqrt(2d + 1/4) - 1/2.
    *)
    Float.to_int (Float.sqrt ((2. *. Float.of_int dist) +. 0.25) -. 0.5)
  in
  let[@tail_mod_cons] rec step_to_point (distx, disty) (vx, vy) =
    (*let () = Printf.eprintf "dist: (%d, %d), vel: (%d, %d)\n" distx disty vx vy in*)
    if distx = 0 && disty = 0 && vx = 0 && vy = 0 then []
    else
      (* Invariant: we're always going in the right direction and never too fast. *)
      let signx = Int.sign distx in
      let signy = Int.sign disty in
      let vabsx' = Int.min (vmax (abs distx)) (abs vx + 1) in
      let vabsy' = Int.min (vmax (abs disty)) (abs vx + 1) in
      let vx' = Sign.to_int signx * vabsx' in
      let vy' = Sign.to_int signy * vabsy' in
      let move = move_of_direction (vx' - vx, vy' - vy) in
      move :: (step_to_point [@tailcall]) (distx - vx', disty - vy') (vx', vy')
  in
  let[@tail_mod_cons] rec to_remaining_points (startx, starty) points =
    match closest_square (startx, starty) points with
    | None -> []
    | Some (endx, endy) ->
        let points =
          (* Remove the point we found from `points`. *)
          List.filter points ~f:(Stdlib.( <> ) (endx, endy))
        in
        let distx = endx - startx in
        let disty = endy - starty in
        let steps = step_to_point (distx, disty) (0, 0) in
        steps :: (to_remaining_points [@tailcall]) (endx, endy) points
  in
  List.concat (to_remaining_points (0, 0) problem)

let simulate problem solution =
  let points_map = Hash_set.Poly.of_list problem in
  Hash_set.Poly.remove points_map (0, 0);
  let _, _, max_speed =
    List.fold solution
      ~init:((0, 0), (0, 0), 0)
      ~f:(fun ((x, y), (dx, dy), max_speed) move ->
        let ax, ay = direction_of_move move in
        let dx = dx + ax in
        let dy = dy + ay in
        let max_speed = Int.max max_speed (Int.max (abs dx) (abs dy)) in
        let x = x + dx in
        let y = y + dy in
        Hash_set.Poly.remove points_map (x, y);
        ((x, y), (dx, dy), max_speed))
  in
  match Hash_set.Poly.to_list points_map with
  | [] ->
      Printf.eprintf "Max speed: %d\n%!" max_speed;
      Printf.eprintf "Length (score): %d\n%!" (List.length solution)
  | points ->
      failwith
        ("Not all points visited: "
        ^ String.concat ~sep:"; " (List.map points ~f:(fun (x, y) -> Printf.sprintf "(%d,%d)" x y))
        )

let () =
  let map_dir, level =
    match Sys.get_argv () with
    | [| _; dir; level |] -> (dir, level)
    | _ -> failwith "Usage: spaceship MAP_DIR LEVEL"
  in
  let problem =
    In_channel.read_lines (map_dir ^ "/spaceship" ^ level ^ ".txt")
    |> List.filter ~f:(String.( <> ) "") (* Skip empty lines *)
    |> List.map ~f:(fun line ->
           match String.split line ~on:' ' with
           | [ key; value ] -> (Int.of_string key, Int.of_string value)
           | _ -> failwith ("Invalid line: " ^ line))
  in
  let sol = solve problem in
  simulate problem sol;
  Solutions.write_solution map_dir level sol;
  print_endline (String.of_char_list sol);
  ()
