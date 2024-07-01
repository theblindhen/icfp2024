open Core
open Util

type direction = R | L | U | D

let dir_of_char = function
  | 'R' -> R
  | 'L' -> L
  | 'U' -> U
  | 'D' -> D
  | _ -> failwith "Invalid direction"

type state = {
  grid : char array array;
  mutable lambdaman : int * int;
  mutable pills : (int * int) Hash_set.Poly.t;
  mutable ticks : int;
}

let step state dir =
  if state.pills |> Hash_set.Poly.is_empty then false
  else
    let cur_x, cur_y = state.lambdaman in
    let next_x, next_y =
      match dir with
      | R -> (cur_x + 1, cur_y)
      | L -> (cur_x - 1, cur_y)
      | U -> (cur_x, cur_y - 1)
      | D -> (cur_x, cur_y + 1)
    in
    state.ticks <- state.ticks + 1;
    if
      (next_x < 0 || next_x >= Array.length state.grid.(0))
      || next_y < 0
      || next_y >= Array.length state.grid
    then true
    else
      let move_lambdaman () =
        state.lambdaman <- (next_x, next_y);
        state.grid.(cur_y).(cur_x) <- ' ';
        state.grid.(next_y).(next_x) <- 'L'
      in
      (match state.grid.(next_y).(next_x) with
      | '#' -> ()
      | '.' ->
          move_lambdaman ();
          Hash_set.Poly.remove state.pills (next_x, next_y)
      | ' ' -> move_lambdaman ()
      | _ -> failwith "Invalid grid");
      true

let find_chars (grid : char array array) c =
  let rec aux x y acc =
    if y = Array.length grid then acc
    else if x = Array.length grid.(y) then aux 0 (y + 1) acc
    else if Stdlib.( = ) grid.(y).(x) c then aux (x + 1) y ((x, y) :: acc)
    else aux (x + 1) y acc
  in
  aux 0 0 []

let run state dirs = List.iter dirs ~f:(fun dir -> step state dir |> ignore)

let run_str (state : state) dirs_s =
  let dirs = dirs_s |> String.to_list |> List.map ~f:dir_of_char in
  run state dirs

let init_state grid =
  let lambdaman = find_chars grid 'L' |> List.hd_exn in
  let pills = find_chars grid '.' |> Hash_set.Poly.of_list in
  let ticks = 0 in
  { grid; ticks; lambdaman; pills }

let duplicate_state state =
  {
    grid = Array.copy_matrix state.grid;
    lambdaman = state.lambdaman;
    pills = Hash_set.Poly.copy state.pills;
    ticks = state.ticks;
  }

let dump_state state =
  let rows_s =
    state.grid
    |> Array.map ~f:(fun row -> row |> Array.to_sequence |> String.of_sequence)
    |> Array.to_list
  in
  "Ticks: " ^ Int.to_string state.ticks ^ "\n" ^ String.concat ~sep:"\n" rows_s
