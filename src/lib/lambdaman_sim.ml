open Core
open Util

type direction = R | L | U | D

type state = {
  grid : char array array;
  mutable lambdaman : int * int;
  mutable pills : (int * int) Hash_set.Poly.t;
}

let step state dir =
  let cur_x, cur_y = state.lambdaman in
  let next_x, next_y =
    match dir with
    | R -> (cur_x + 1, cur_y)
    | L -> (cur_x - 1, cur_y)
    | U -> (cur_x, cur_y + 1)
    | D -> (cur_x, cur_y - 1)
  in
  if
    (next_x < 0 || next_x >= Array.length state.grid.(0))
    || next_y < 0
    || next_y >= Array.length state.grid
  then ()
  else
    match state.grid.(next_y).(next_x) with
    | '#' -> ()
    | '.' ->
        state.lambdaman <- (next_x, next_y);
        Hash_set.Poly.remove state.pills (next_x, next_y)
    | ' ' -> state.lambdaman <- (next_x, next_y)
    | _ -> failwith "Invalid grid"

let find_chars (grid : char array array) c =
  let rec aux x y acc =
    if y = Array.length grid then acc
    else if x = Array.length grid.(y) then aux 0 (y + 1) acc
    else if Stdlib.( = ) grid.(y).(x) c then aux (x + 1) y ((x, y) :: acc)
    else aux (x + 1) y acc
  in
  aux 0 0 []

let init_state grid =
  let lambdaman = find_chars grid 'L' |> List.hd_exn in
  let pills = find_chars grid '.' |> Hash_set.Poly.of_list in
  { grid; lambdaman; pills }

let dump_state state =
  let rows_s =
    state.grid
    |> Array.map ~f:(fun row -> row |> Array.to_sequence |> String.of_sequence)
    |> Array.to_list
  in
  String.concat ~sep:"\n" rows_s
