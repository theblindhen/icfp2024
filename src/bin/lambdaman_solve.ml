open Core
open Lib
open Astar.Astar

(* Define the directions *)
let directions = [| (0, 1, 'R'); (0, -1, 'L'); (1, 0, 'D'); (-1, 0, 'U') |]
(* Define the type of the problem *)

(* Read the grid from a file *)
let read_grid filename =
  let in_channel = In_channel.create filename in
  let rec aux grid =
    try
      let line = In_channel.input_line_exn in_channel in
      aux (line :: grid)
    with
    | End_of_file ->
        In_channel.close in_channel;
        List.rev grid
  in
  Array.of_list (aux [])

(* Find the starting position of Lambda-Man and the positions of all pills *)
let find_positions grid =
  (* Debug *)
  let lambda_pos = ref (-1, -1) in
  let pills = ref [] in
  for i = 0 to Array.length grid - 1 do
    for j = 0 to String.length grid.(i) - 1 do
      match grid.(i).[j] with
      | 'L' -> lambda_pos := (i, j)
      | '.' -> pills := (i, j) :: !pills
      | _ -> ()
    done
  done;
  (!lambda_pos, !pills)

(* Shortest path between start and goal using A* *)
let find_shortest_path grid start goal =
  (* Debug *)
  let grid_width = String.length grid.(0) in
  let grid_height = Array.length grid in
  let problem : 'a t =
    {
      cost = (fun (x1, y1) (x2, y2) -> abs (x1 - x2) + abs (y1 - y2));
      get_next_states =
        (fun (i, j) ->
          Array.fold directions ~init:[] ~f:(fun acc (di, dj, _) ->
              let i', j' = (i + di, j + dj) in
              if
                i' >= 0
                && i' < grid_height
                && j' >= 0
                && j' < grid_width
                && Char.(grid.(i').[j'] <> '#')
              then (i', j') :: acc
              else acc));
      goal;
    }
  in
  let path = List.rev (search problem start) in
  let rec directions_from_path = function
    | [] -> []
    | [ _ ] -> []
    | (i1, j1) :: (i2, j2) :: tl ->
        let dir =
          match (i2 - i1, j2 - j1) with
          | 1, 0 -> 'D'
          | -1, 0 -> 'U'
          | 0, 1 -> 'R'
          | 0, -1 -> 'L'
          | _ -> raise (Invalid_argument "Invalid path")
        in
        dir :: directions_from_path ((i2, j2) :: tl)
  in
  directions_from_path path

(* Find the shortest path to visit all pills using a greedy algorithm with A* *)
let solve_traveling_lambdaman grid start pills =
  let positions = List.to_array (start :: pills) in
  let n = Array.length positions in
  let visited = Array.create ~len:n false in
  let path = ref [] in
  let current_pos = ref 0 in
  visited.(!current_pos) <- true;
  for _ = 1 to n - 1 do
    let next_pos = ref (-1) in
    let min_dist = ref Int.max_value in
    for i = 0 to n - 1 do
      if (not visited.(i)) && i <> !current_pos then
        let p = find_shortest_path grid positions.(!current_pos) positions.(i) in
        if List.length p < !min_dist then (
          next_pos := i;
          min_dist := List.length p)
    done;
    (* let _ =
         print_endline
           (sprintf "%s %s %s\n"
              (Sexp.to_string_hum
                 (Tuple2.sexp_of_t Int.sexp_of_t Int.sexp_of_t positions.(!current_pos)))
              (Sexp.to_string_hum (Tuple2.sexp_of_t Int.sexp_of_t Int.sexp_of_t positions.(!next_pos)))
              (String.of_char_list
                 (find_shortest_path grid positions.(!current_pos) positions.(!next_pos))))
       in *)
    path := !path @ find_shortest_path grid positions.(!current_pos) positions.(!next_pos);
    visited.(!next_pos) <- true;
    current_pos := !next_pos
  done;
  !path

(* Main function *)
let () =
  if Array.length (Sys.get_argv ()) <> 2 then
    printf "Usage: %s <input_file>\n" (Sys.get_argv ()).(0)
  else
    let filename = (Sys.get_argv ()).(1) in
    let grid = read_grid filename in
    let lambda_pos, pills = find_positions grid in
    let path = solve_traveling_lambdaman grid lambda_pos pills in
    List.iter ~f:(printf "%c") path;
    printf "\n"
