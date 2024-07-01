open Core
open Lib
open Astar

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
  | _ -> failwith (sprintf "Can't move this far in one step: %d,%d" ax ay)

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

type state = int * int * int * int

let string_of_state (x, y, vx, vy) = Printf.sprintf "(%d,%d,%d,%d)" x y vx vy

let solve (problem : (int * int) list) =
  (* Solution strategy:
      - While where are squares to visit:
        - Pick the closest square
        - Move towards it with both vx and vy between -1 and 1 (later: larger values)
        - Stop when we reach it (later: keep moving)
  *)
  let max_stoppable_speed dist =
    (*
      How much can we speed up when there's distance dist left, assuming we need
      to slow down to a standstill before we get there?

      Let's say we're considering whether to move at speed 3. Then we must go
      3+2+1=6 steps before we're stopped. Taking that backwards, if we're at distance 6
      from the target, we can speed up to 3.
      Generally, d >= v + (v-1) + ... + 0 = v*(v+1)/2, so v <= sqrt(2d + 1/4) - 1/2.
    *)
    Float.to_int (Float.sqrt ((2. *. Float.of_int dist) +. 0.25) -. 0.5)
  in
  let rec step_to_point (distx, disty) (vx, vy) moves_rev =
    (*let () = Printf.eprintf "dist: (%d, %d), vel: (%d, %d)\n" distx disty vx vy in*)
    if distx = 0 && disty = 0 then (vx, vy, List.rev moves_rev)
    else
      let signx = Int.sign distx in
      let signy = Int.sign disty in
      let vx' =
        if distx <> 0 && Sign.(signx <> Sign.of_int vx) then
          (* We've overshot, and we have to go backwards *)
          vx + Sign.to_int signx
        else
          (* Go forward as fast as possible *)
          Sign.to_int signx * max_stoppable_speed (abs distx) |> max (vx - 1) |> min (vx + 1)
      in
      let vy' =
        if disty <> 0 && Sign.(signy <> Sign.of_int vy) then
          (* We've overshot, and we have to go backwards *)
          vy + Sign.to_int signy
        else
          (* Go forward as fast as possible *)
          Sign.to_int signy * max_stoppable_speed (abs disty) |> max (vy - 1) |> min (vy + 1)
      in
      let move = move_of_direction (vx' - vx, vy' - vy) in
      step_to_point (distx - vx', disty - vy') (vx', vy') (move :: moves_rev)
  in
  (* MUTABLE: remaining points *)
  let point_set = Hash_set.Poly.of_list problem in
  Hash_set.Poly.remove point_set (0, 0);
  (* Alternative implementation: search the local neighborhood of moves.
     Consider all states we can get into by making `depth` moves and see if
     they happen to hit upon a point. *)
  let rec search_one_point ~depth (frontier : (state * char list) list) =
    (* Invariant: nothing in frontier is in point_set. *)
    if depth = 0 then None
    else
      let frontier =
        List.concat_map frontier ~f:(fun ((x, y, vx, vy), moves) ->
            List.map [ '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' ] ~f:(fun move ->
                (* TODO: there are some easy options for excluding silly moves:
                   - Don't stand still.
                   - Don't move in a direction where there's no target.
                   - Limit speed by max_stoppable_speed or something similar.
                   - Don't revisit a state we've already seen (but this
                     requires memory, and maybe it doesn't help all that much).
                *)
                let ax, ay = direction_of_move move in
                let vx' = vx + ax in
                let vy' = vy + ay in
                let x' = x + vx' in
                let y' = y + vy' in
                let moves' = move :: moves in
                ((x', y', vx', vy'), moves')))
      in
      (* If something in the frontier matches a point, pick that as the solution.
         TODO: Prefer nearer targets or something like that? *)
      match List.find frontier ~f:(fun ((x, y, _, _), _) -> Hash_set.Poly.mem point_set (x, y)) with
      | Some (state, moves) -> Some (state, List.rev moves)
      | None -> search_one_point ~depth:(depth - 1) frontier
  in
  let[@tail_mod_cons] rec to_remaining_points (startx, starty, vx, vy) =
    match closest_square (startx, starty) (Hash_set.Poly.to_list point_set) with
    | None -> []
    | Some (endx, endy) ->
        Hash_set.Poly.remove point_set (endx, endy);
        let distx = endx - startx in
        let disty = endy - starty in
        let vx, vy, steps = step_to_point (distx, disty) (vx, vy) [] in
        steps :: (to_remaining_points [@tailcall]) (endx, endy, vx, vy)
  and search_all_points (state : state) =
    match search_one_point ~depth:7 [ (state, []) ] with
    | None -> to_remaining_points state
    | Some (((x, y, _, _) as state), moves) ->
        Hash_set.remove point_set (x, y);
        if Hash_set.is_empty point_set then [ moves ] else moves :: search_all_points state
  in
  List.concat (search_all_points (0, 0, 0, 0))

type astar_state = state * bool (* pos, vel, took_first *)

let best_path_2 state first next =
  let fly_dist (x, y, vx, vy) (x', y') =
    let composant_dist (z, vz) z' =
      let dz = z' - z in
      (* TODO: Improve heuristic by not computing in abs vz *)
      let rec try_t t = if (t * abs vz) + (t * t) >= dz then t else try_t (t + 1) in
      try_t 1
    in
    max (composant_dist (x, vx) x') (composant_dist (y, vy) y')
  in
  let astar_problem : astar_state problem =
    {
      move_cost = (fun _ _ -> 1);
      is_goal = (fun ((x, y, _, _), took_first) -> took_first && Stdlib.((x, y) = next));
      get_next_states =
        (fun ((x, y, vx, vy), took_first) ->
          List.map [ '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9' ] ~f:(fun move ->
              let ax, ay = direction_of_move move in
              let vx' = vx + ax in
              let vy' = vy + ay in
              let x' = x + vx' in
              let y' = y + vy' in
              ((x', y', vx', vy'), took_first || Stdlib.((x', y') = first))));
      heuristic_cost =
        (fun (pos_vel, took_first) ->
          if took_first then fly_dist pos_vel next
          else
            let _, _, vx, vy = pos_vel in
            let fx, fy = first in
            let first_t = fly_dist pos_vel first in
            first_t + fly_dist (fx, fy, vx + first_t, vy + first_t) next);
    }
  in
  let to_state (state, _) = state in
  let positions = Astar.search astar_problem (state, false) |> List.rev |> List.tl_exn in
  printf "Found solution: From %s with [%s]\n%!" (string_of_state state)
    (positions
    |> List.map ~f:to_state
    |> List.map ~f:(fun (x, y, vx, vy) -> Printf.sprintf "(%d,%d,%d,%d)" x y vx vy)
    |> String.concat ~sep:"; ");
  let first, next =
    let did_take_first (_, took_first) = not took_first in
    let took_first = List.take_while positions ~f:did_take_first in
    let didnt = List.drop_while positions ~f:did_take_first in
    ( took_first @ [ List.hd_exn didnt ] |> List.map ~f:to_state,
      List.tl_exn didnt |> List.map ~f:to_state )
  in
  let to_moves init_state states =
    let rec aux ((x, y, vx, vy) as cur) rest =
      match rest with
      | [] -> []
      | ((nx, ny, _, _) as next) :: tl ->
          printf "Converting move from %s to %d,%d\n%!" (string_of_state cur) nx ny;
          let move = move_of_direction (nx - x - vx, ny - y - vy) in
          move :: aux next tl
    in
    aux init_state states
  in
  let after_first = List.last_exn first in
  (to_moves state first, to_moves after_first next, after_first)

let line_solve (problem : (int * int) list) =
  (* Solution strategy:
      - Assume the list of points is sorted as a line (later, sort it)
      - Find the shortes moves that reach the first two points.
      - Keep only the points until the next point; recurse
  *)
  let rec aux state points acc =
    match points with
    | [] -> acc
    | _ :: [] -> failwith "Can't solve a problem of len 1"
    | first :: next :: tl ->
        let x, y, _, _ = state in
        printf "Moving from %d,%d to %d,%d and then %d,%d\n%!" x y (fst first) (snd first)
          (fst next) (snd next);
        let to_first, to_next, state = best_path_2 state first next in
        printf "To first: [%s]\n" (String.of_char_list to_first);
        if List.is_empty tl then acc @ to_first @ to_next
        else aux state (next :: tl) (acc @ to_first)
  in
  aux (0, 0, 0, 0) problem []

let simulate problem solution =
  let point_set = Hash_set.Poly.of_list problem in
  Hash_set.Poly.remove point_set (0, 0);
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
        Hash_set.Poly.remove point_set (x, y);
        ((x, y), (dx, dy), max_speed))
  in
  match Hash_set.Poly.to_list point_set with
  | [] ->
      Printf.eprintf "Max speed: %d\n%!" max_speed;
      Printf.eprintf "Length (score): %d\n%!" (List.length solution)
  | points ->
      failwith
        ("Not all points visited: "
        ^ String.concat ~sep:"; " (List.map points ~f:(fun (x, y) -> Printf.sprintf "(%d,%d)" x y))
        )

let () =
  let map_dir, level, line_solver =
    match Sys.get_argv () with
    | [| _; "-l"; dir; level |] -> (dir, level, true)
    | [| _; dir; level |] -> (dir, level, false)
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
  let sol = if line_solver then line_solve problem else solve problem in
  simulate problem sol;
  let sol_str = String.of_char_list sol in
  Solutions.write_solution "spaceship" map_dir level sol_str;
  print_endline sol_str;
  ()
