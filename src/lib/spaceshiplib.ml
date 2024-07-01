open Core
open Astar

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

(** An optimistic heuristic for the number of time steps to fly from (x,y) to (x',y') *)
let fly_dist (x, y, vx, vy) (x', y') =
  let composant_dist (z, vz) z' =
    let dz = abs (z' - z) in
    let is_non_neg a = if a < 0 then -1 else 1 in
    let vz_signed = is_non_neg dz * is_non_neg vz * vz in
    let rec try_t t = if (t * vz_signed) + (t * t / 2) + 1 >= dz then t else try_t (t + 1) in
    try_t 1
  in
  if x = x' && y = y' then 0 else max (composant_dist (x, vx) x') (composant_dist (y, vy) y')

type astar_state = state * bool (* pos, vel, took_first *)

let best_path_2 state first next =
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

(* TESTS *)
let%test_unit "fly_dist" =
  let t = fly_dist (0, 0, 0, 0) (0, 0) in
  printf "%d\n" t;
  assert (t = 0)

let%test_unit "fly_dist2" =
  let t = fly_dist (-32, -115, -5, -7) (-45, -127) in
  printf "%d\n" t;
  assert (t = 2)
