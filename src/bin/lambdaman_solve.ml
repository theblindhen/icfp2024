open Core
open Lib
open Astar.Astar

(* Define the directions *)
let directions = [| (0, 1, 'R'); (0, -1, 'L'); (1, 0, 'D'); (-1, 0, 'U') |]
(* Define the type of the problem *)

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
      move_cost = Util.manhattan_distance;
      heuristic_cost = Util.manhattan_distance goal;
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
      is_goal = (fun pos -> Stdlib.(pos = goal));
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

let flood_find_nearest_pill (grid : char array array) (start : int * int) : int * int =
  let grid_width = Array.length grid.(0) in
  let grid_height = Array.length grid in
  let visited = Array.make_matrix ~dimx:grid_height ~dimy:grid_width false in
  let queue = Queue.create () in
  Queue.enqueue queue start;
  visited.(fst start).(snd start) <- true;
  let found = ref None in
  while not (Queue.is_empty queue) do
    let i, j = Queue.dequeue_exn queue in
    if Stdlib.( = ) grid.(i).(j) '.' then (
      found := Some (i, j);
      Queue.clear queue)
    else
      Array.iter directions ~f:(fun (di, dj, _) ->
          let i', j' = (i + di, j + dj) in
          if
            i' >= 0
            && i' < grid_height
            && j' >= 0
            && j' < grid_width
            && Char.(grid.(i').(j') <> '#')
            && not visited.(i').(j')
          then (
            visited.(i').(j') <- true;
            Queue.enqueue queue (i', j')))
  done;
  match !found with
  | Some p -> p
  | None -> failwith "No pill found"

(* Find the shortest path to visit all pills using a greedy algorithm with A* *)
let solve_traveling_lambdaman grid start pills =
  let path = ref [] in
  let (* mut *) char_grid = Util.to_char_array grid in
  let current_pos = ref start in
  for _ = 1 to List.length pills do
    let next_pos = flood_find_nearest_pill char_grid !current_pos in
    let next_path = find_shortest_path grid !current_pos next_pos in
    (* let _ =
         print_endline
           (sprintf "%s %s %s\n"
              (Sexp.to_string_hum
                 (Tuple2.sexp_of_t Int.sexp_of_t Int.sexp_of_t positions.(!current_pos)))
              (Sexp.to_string_hum (Tuple2.sexp_of_t Int.sexp_of_t Int.sexp_of_t positions.(!next_pos)))
              (String.of_char_list
                 (find_shortest_path grid positions.(!current_pos) positions.(!next_pos))))
       in *)
    path := !path @ next_path;
    char_grid.(fst next_pos).(snd next_pos) <- ' ';
    current_pos := next_pos
  done;
  !path

(* Main function *)
let () =
  if Array.length (Sys.get_argv ()) <> 3 then
    printf "Usage: %s <input_dir> <level>\n" (Sys.get_argv ()).(0)
  else
    let dir = (Sys.get_argv ()).(1) in
    let level = (Sys.get_argv ()).(2) in
    let filename = dir ^ "/lambdaman" ^ level ^ ".txt" in
    let grid = Util.read_grid filename in
    let lambda_pos, pills = find_positions grid in
    let path = solve_traveling_lambdaman grid lambda_pos pills in
    List.iter ~f:(printf "%c") path;
    printf "\n";
    let sol_str = String.of_char_list path in
    let sol_int = Lambdaman_pack.encode_dirs sol_str in
    let prefix = "solve lambdaman" ^ level ^ " " in
    let pack_sol =
      Language.deparse
        Metalanguage.(concat_op (String prefix) (app Lambdaman_pack.decode_dirs (Integer sol_int)))
    in
    let nonpack_sol = Language.encode_string_token (prefix ^ String.of_char_list path) in
    let rep_sol =
      Language.deparse
        Metalanguage.(concat_op (String prefix) (Lambdaman_pack.encode_as_repeats sol_str))
    in
    let sols =
      [ ("pack", pack_sol); ("string", nonpack_sol); ("repeat", rep_sol) ]
      |> List.map ~f:(fun (name, sol) -> (name, sol, String.length sol))
    in
    List.iter sols ~f:(fun (name, sol, size) ->
        printf "%s yielded encoding of size %d\n" name size;
        Solutions.write_solution "lambdaman" dir level sol)
