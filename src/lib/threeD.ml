open Core
open Util

type binop = Plus | Minus | Equal | NotEqual | Mult | Div | Mod

let binop_to_string = function
  | Plus -> "+"
  | Minus -> "-"
  | Equal -> "="
  | NotEqual -> "#"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"

type placeholder = A | B | S

let placeholder_to_string = function
  | A -> "A"
  | B -> "B"
  | S -> "S"

type shift_dir = Left | Right | Up | Down

let shift_dir_to_string = function
  | Left -> "<"
  | Right -> ">"
  | Up -> "^"
  | Down -> "v"

type cell =
  | (* empty *) Empty
  | Literal of Bigint.t
  | Shift of shift_dir
  | Binop of binop
  | Timewarp

type parse_cell = Cell of cell | Place of placeholder

let cell_to_string = function
  | Empty -> "."
  | Literal i -> Bigint.to_string_hum i
  | Shift d -> shift_dir_to_string d
  | Binop b -> binop_to_string b
  | Timewarp -> "@"

let parse_cell_to_string = function
  | Cell c -> cell_to_string c
  | Place p -> placeholder_to_string p

type grid = cell array array
type parse_grid = parse_cell array array

let _grid_to_string (cell_to_string : int * int -> 'a -> string) (grid : 'a array array) =
  (* First map each cell to its string *)
  let grid_strs =
    Array.mapi grid ~f:(fun y row -> Array.mapi row ~f:(fun x cell -> cell_to_string (x, y) cell))
  in
  (* Then find the max width of each column *)
  let col_widths = Array.init (Array.length grid_strs.(0)) ~f:(fun _ -> 0) in
  for x = 0 to Array.length grid_strs.(0) - 1 do
    for y = 0 to Array.length grid_strs - 1 do
      col_widths.(x) <- Int.max col_widths.(x) (String.length grid_strs.(y).(x))
    done
  done;
  (* Finally, concatenate each row with padding *)
  let rows =
    Array.map grid_strs ~f:(fun row ->
        Array.foldi row ~init:"" ~f:(fun i acc cell ->
            acc ^ " " ^ String.make (col_widths.(i) - String.length cell) ' ' ^ cell))
  in
  Array.fold rows ~init:"" ~f:(fun acc row -> acc ^ row ^ "\n\n")

let grid_to_string = _grid_to_string (fun _ c -> cell_to_string c)
let parse_grid_to_string = _grid_to_string (fun _ c -> parse_cell_to_string c)

let grid_to_string_with_s grid s_pos =
  let s_set = Hash_set.Poly.create () in
  List.iter s_pos ~f:(fun (x, y) -> Hash_set.add s_set (x, y));
  _grid_to_string
    (fun (x, y) c ->
      match c with
      | Empty -> if Hash_set.mem s_set (x, y) then "S" else cell_to_string c
      | _ -> cell_to_string c)
    grid

type state = {
  grid : grid;
  s_pos : (int * int) list;
  snapshots : (int * grid) list;
  max_dims : int * int;
  current_time : int;
  current_ticks : int;
  return_value : Bigint.t option;
}

let parse_grid (input : string) : parse_grid =
  let rows = String.split_lines input |> List.map ~f:String.strip |> Array.of_list in
  let grid =
    Array.map rows ~f:(fun row ->
        let tokens =
          String.split ~on:' ' row |> List.filter ~f:(fun s -> not (String.is_empty s))
        in
        List.map tokens ~f:(fun token ->
            match token with
            | "." -> Cell Empty
            | "@" -> Cell Timewarp
            | "<" -> Cell (Shift Left)
            | ">" -> Cell (Shift Right)
            | "^" -> Cell (Shift Up)
            | "v" -> Cell (Shift Down)
            | "+" -> Cell (Binop Plus)
            | "-" -> Cell (Binop Minus)
            | "=" -> Cell (Binop Equal)
            | "#" -> Cell (Binop NotEqual)
            | "*" -> Cell (Binop Mult)
            | "/" -> Cell (Binop Div)
            | "%" -> Cell (Binop Mod)
            | "A" -> Place A
            | "B" -> Place B
            | "S" -> Place S
            | x -> (
                (* try to get an int *)
                try Cell (Literal (Bigint.of_string x)) with
                | _ -> failwith ("Invalid token: " ^ x)))
        |> Array.of_list)
  in
  (* assert all rows are equal length *)
  let row_length = Array.length grid.(0) in
  if not (Array.for_all grid ~f:(fun row -> Array.length row = row_length)) then
    failwith "All rows must be the same length";
  grid

let apply_placeholders (parse_grid : parse_grid) (a : Bigint.t) (b : Bigint.t) =
  let s_pos = ref [] in
  let grid =
    Array.mapi parse_grid ~f:(fun y row ->
        Array.mapi row ~f:(fun x cell ->
            match cell with
            | Place A -> Literal a
            | Place B -> Literal b
            | Place S ->
                s_pos := (x, y) :: !s_pos;
                Empty
            | Cell c -> c))
  in
  (grid, !s_pos)

let init_state (input : string) (a : Bigint.t) (b : Bigint.t) =
  let parse_grid = parse_grid input in
  let grid, s_pos = apply_placeholders parse_grid a b in
  let max_dims = (Array.length grid.(0), Array.length grid) in
  let snapshots = [ (0, Array.copy_matrix grid) ] in
  let current_time = 0 in
  let current_ticks = 0 in
  let return_value = None in
  { grid; s_pos; snapshots; max_dims; current_time; current_ticks; return_value }

let dump_state (state : state) : string =
  let time = sprintf "Time: %6d Ticks: %6d\n" state.current_time state.current_ticks in
  let return_value =
    match state.return_value with
    | None -> ""
    | Some v -> "Return value: " ^ Bigint.to_string_hum v ^ "\n"
  in
  time ^ return_value ^ grid_to_string_with_s state.grid state.s_pos

let rec step (state : state) =
  (* make a hashmap of int*int -> new cell values *)
  if Stdlib.( <> ) state.return_value None then state
  else
    (* Find the actions *)
    let actions = Hashtbl.Poly.create () in
    let timewarp = ref None in
    let set_action pos cell =
      (* printf "Setting action at (%d, %d) to %s\n" (fst pos) (snd pos) (cell_to_string cell); *)
      match Hashtbl.find actions pos with
      | None
      | Some Empty ->
          Hashtbl.set actions ~key:pos ~data:cell
      | Some _ ->
          if Stdlib.( = ) cell Empty then ()
          else failwith "Multiple cells trying to occupy the same position"
    in
    let out_of_upper_left (x, y) = x < 0 || y < 0 in
    let out_of_bounds (x, y) =
      (* printf "Checking bounds (%d, %d)\n" x y; *)
      out_of_upper_left (x, y) || x >= fst state.max_dims || y >= snd state.max_dims
    in
    Array.iteri state.grid ~f:(fun y row ->
        Array.iteri row ~f:(fun x cell ->
            let shift_check ~from ~to_ =
              if out_of_bounds from || out_of_upper_left to_ then
                (* printf "Out of bounds (%d, %d) to (%d, %d)\n" (fst from) (snd from) (fst to_)
                   (snd to_); *)
                ()
              else if out_of_bounds to_ then raise (Failure "Not implemented: Grow the grid")
              else
                match state.grid.(snd from).(fst from) with
                | Empty -> ()
                | c ->
                    set_action to_ c;
                    set_action from Empty
            in
            match cell with
            | Empty
            | Literal _ ->
                ()
            | Shift Right -> shift_check ~from:(x - 1, y) ~to_:(x + 1, y)
            | Shift Left -> shift_check ~from:(x + 1, y) ~to_:(x - 1, y)
            | Shift Up -> shift_check ~from:(x, y + 1) ~to_:(x, y - 1)
            | Shift Down -> shift_check ~from:(x, y - 1) ~to_:(x, y + 1)
            | Binop op -> (
                if
                  (* printf "Binop at (%d, %d), bounds %d x %d\n" x y (fst state.max_dims)
                     (snd state.max_dims); *)
                  out_of_upper_left (x - 1, y - 1)
                then ()
                else
                  match (state.grid.(y).(x - 1), state.grid.(y - 1).(x)) with
                  | Literal left, Literal above -> (
                      let same r = (r, r) in
                      let res_right_and_down =
                        match op with
                        | Plus -> Some (Literal Bigint.(left + above) |> same)
                        | Minus -> Some (Literal Bigint.(left - above) |> same)
                        | Mult -> Some (Literal Bigint.(left * above) |> same)
                        | Div -> Some (Literal Bigint.(left / above) |> same)
                        | Mod -> Some (Literal (Bigint.rem left above) |> same)
                        | Equal ->
                            if Bigint.(left = above) then Some (Literal left |> same) else None
                        | NotEqual ->
                            (* above goes right, and left goes down *)
                            if Bigint.(left <> above) then Some (Literal above, Literal left)
                            else None
                      in
                      match res_right_and_down with
                      | None -> ()
                      | Some (right, down) ->
                          if out_of_bounds (x + 1, y + 1) then
                            raise
                              (Failure
                                 (sprintf
                                    "Not implemented: Grow the grid to (%d, %d) beyond (%d, %d)"
                                    (x + 1) (y + 1) (fst state.max_dims) (snd state.max_dims)))
                          else (
                            set_action (x + 1, y) right;
                            set_action (x, y + 1) down;
                            set_action (x - 1, y) Empty;
                            set_action (x, y - 1) Empty))
                  | _ -> ())
            | Timewarp -> (
                if out_of_upper_left (x - 1, y - 1) then ()
                else if out_of_bounds (x + 1, y + 1) then
                  raise
                    (Failure
                       (sprintf "Not implemented: Timewarp at the edge of the grid (%d, %d)" x y))
                else
                  match
                    ( state.grid.(y - 1).(x),
                      state.grid.(y).(x - 1),
                      state.grid.(y).(x + 1),
                      state.grid.(y + 1).(x) )
                  with
                  | Literal v, Literal dx, Literal dy, Literal dt -> (
                      match !timewarp with
                      | None -> timewarp := Some (small dt, [ (v, x - small dx, y - small dy) ])
                      | Some (dt', _) when small dt <> dt' ->
                          raise
                            (Failure
                               (sprintf "Multiple timewarps with different dt: %d vs %d" (small dt)
                                  dt'))
                      | Some (_, l) ->
                          timewarp := Some (small dt, (v, x - small dx, y - small dy) :: l))
                  | _ -> ())));

    (* apply the actions *)
    Hashtbl.iteri actions ~f:(fun ~key:(x, y) ~data:cell -> state.grid.(y).(x) <- cell);

    (* Check for return values *)
    let return_value = ref None in
    List.iter state.s_pos ~f:(fun (x, y) ->
        match state.grid.(y).(x) with
        | Literal i -> (
            match !return_value with
            | None -> return_value := Some i
            | Some v ->
                if Bigint.(i <> v) then
                  raise
                    (Failure
                       (sprintf "Multiple return values: %s and %s" (Bigint.to_string_hum i)
                          (Bigint.to_string_hum v)))
                else return_value := Some i)
        | _ -> ());

    match !timewarp with
    | None ->
        {
          state with
          current_time = state.current_time + 1;
          current_ticks = state.current_ticks + 1;
          return_value = !return_value;
        }
    | Some (dt, warps) ->
        (* apply timewarp *)
        let target_time = state.current_time - dt in
        let rewound_snapshots =
          match List.drop_while state.snapshots ~f:(fun (t, _) -> t > target_time) with
          | [] -> raise (Failure "No snapshot to rewind to")
          | ls -> ls
        in
        let snapshot_time, snapshot = List.hd_exn rewound_snapshots in
        let replay_ticks = target_time - snapshot_time in
        let snapshot_state =
          {
            state with
            grid = Array.copy_matrix snapshot;
            current_time = snapshot_time;
            current_ticks = state.current_ticks - replay_ticks;
            snapshots = rewound_snapshots;
          }
        in
        let new_old_state = step_mult replay_ticks snapshot_state in
        List.iter warps ~f:(fun (v, x, y) -> new_old_state.grid.(y).(x) <- Literal v);
        {
          new_old_state with
          current_time = new_old_state.current_time;
          current_ticks = new_old_state.current_ticks;
        }

and step_mult n state =
  if n <= 0 then state
  else
    let new_state = step state in
    step_mult (n - 1) new_state

(* TESTS *)
let assert_equal_grids (s1 : state) (s2 : state) =
  let grid1 = grid_to_string s1.grid in
  let grid2 = grid_to_string s2.grid in
  if not (String.equal grid1 grid2) then (
    eprintf "Grids not equal:\nGot:\n%s\nExpected:\n%s" grid1 grid2;
    failwith "Grids not equal")

let _init_state input = init_state input (Bigint.of_int 88) (Bigint.of_int 99)

let%test_unit "shift_left" =
  let state = _init_state ". < 1" |> step in
  let expected = _init_state "1 < ." in
  assert_equal_grids state expected

let%test_unit "shift_left_overwrite" =
  let state = _init_state "+ < 1" |> step in
  let expected = _init_state "1 < ." in
  assert_equal_grids state expected

let%test_unit "shift_right" =
  let state = _init_state "1 > ." |> step in
  let expected = _init_state ". > 1" in
  assert_equal_grids state expected

let%test_unit "two_way_shift" =
  let state = _init_state ". < 1 > ." |> step in
  let expected = _init_state "1 < . > 1" in
  assert_equal_grids state expected

let%test_unit "train_left" =
  let state = _init_state ". < 1 < 2" |> step in
  let expected = _init_state "1 < 2 < ." in
  assert_equal_grids state expected

let%test_unit "train_right" =
  let state = _init_state "1 > 2 > ." |> step in
  let expected = _init_state ". > 1 > 2" in
  assert_equal_grids state expected

let%test_unit "shift_down" =
  let state =
    "  1                                                  \n"
    ^ "v                                                  \n"
    ^ ".                                                  \n"
    |> _init_state
    |> step
  in
  let expected =
    "  .                                                  \n"
    ^ "v                                                  \n"
    ^ "1                                                  \n"
    |> _init_state
  in
  assert_equal_grids state expected

let%test_unit "shift_up" =
  let state =
    "  .                                                  \n"
    ^ "^                                                  \n"
    ^ "1                                                  \n"
    |> _init_state
    |> step
  in
  let expected =
    "  1                                                  \n"
    ^ "^                                                  \n"
    ^ ".                                                  \n"
    |> _init_state
  in
  assert_equal_grids state expected

let%test_unit "add" =
  let state =
    "  . 1 .                                               \n"
    ^ "2 + .                                               \n"
    ^ ". . .                                              \n"
    |> _init_state
    |> step
  in
  let expected =
    "  . . .                                              \n"
    ^ ". + 3                                              \n"
    ^ ". 3 .                                              \n"
    |> _init_state
  in
  assert_equal_grids state expected

let%test_unit "minus" =
  let state =
    "  . 1 .                                               \n"
    ^ "3 - .                                               \n"
    ^ ". . .                                              \n"
    |> _init_state
    |> step
  in
  let expected =
    "  . . .                                              \n"
    ^ ". - 2                                              \n"
    ^ ". 2 .                                              \n"
    |> _init_state
  in
  assert_equal_grids state expected

let%test_unit "multiply" =
  let state =
    "  . 2 .                                               \n"
    ^ "3 * .                                               \n"
    ^ ". . .                                              \n"
    |> _init_state
    |> step
  in
  let expected =
    "  . . .                                              \n"
    ^ ". * 6                                              \n"
    ^ ". 6 .                                              \n"
    |> _init_state
  in
  assert_equal_grids state expected

let%test_unit "division" =
  let state =
    "  . 2 .                                               \n"
    ^ "3 / .                                               \n"
    ^ ". . .                                              \n"
    |> _init_state
    |> step
  in
  let expected =
    "  . . .                                              \n"
    ^ ". / 1                                              \n"
    ^ ". 1 .                                              \n"
    |> _init_state
  in
  assert_equal_grids state expected

let%test_unit "division_neg" =
  let state =
    "   . 3 .                                               \n"
    ^ "-8 / .                                               \n"
    ^ " . . .                                               \n"
    |> _init_state
    |> step
  in
  let expected =
    "  .  .  .                                              \n"
    ^ ".  / -2                                              \n"
    ^ ". -2  .                                              \n"
    |> _init_state
  in
  assert_equal_grids state expected

let%test_unit "division_neg2" =
  let state =
    "   . -3 .                                               \n"
    ^ " 8  / .                                               \n"
    ^ " .  . .                                               \n"
    |> _init_state
    |> step
  in
  let expected =
    "  .  .  .                                              \n"
    ^ ".  / -2                                              \n"
    ^ ". -2  .                                              \n"
    |> _init_state
  in
  assert_equal_grids state expected

let%test_unit "mod" =
  let state =
    "   . 3 .                                               \n"
    ^ " 8 % .                                               \n"
    ^ " . . .                                               \n"
    |> _init_state
    |> step
  in
  let expected =
    "  .  .  .                                              \n"
    ^ ".  %  2                                              \n"
    ^ ".  2  .                                              \n"
    |> _init_state
  in
  assert_equal_grids state expected

let%test_unit "mod_neg" =
  let state =
    "   . 3 .                                               \n"
    ^ "-8 % .                                               \n"
    ^ " . . .                                               \n"
    |> _init_state
    |> step
  in
  let expected =
    "  .  .  .                                              \n"
    ^ ".  % -2                                              \n"
    ^ ". -2  .                                              \n"
    |> _init_state
  in
  assert_equal_grids state expected

let%test_unit "mod_neg" =
  let state =
    "   . -3 .                                               \n"
    ^ " 8  % .                                               \n"
    ^ " .  . .                                               \n"
    |> _init_state
    |> step
  in
  let expected =
    "  .  .  .                                              \n"
    ^ ".  %  2                                              \n"
    ^ ".  2  .                                              \n"
    |> _init_state
  in
  assert_equal_grids state expected

let%test_unit "eq_fires" =
  let state =
    "   . 3 .                                               \n"
    ^ " 3 = .                                               \n"
    ^ " . . .                                               \n"
    |> _init_state
    |> step
  in
  let expected =
    "  .  .  .                                              \n"
    ^ ".  =  3                                              \n"
    ^ ".  3  .                                              \n"
    |> _init_state
  in
  assert_equal_grids state expected

let%test_unit "eq_unfired" =
  let state =
    "   . 1 .                                               \n"
    ^ " 3 = .                                               \n"
    ^ " . . .                                               \n"
    |> _init_state
    |> step
  in
  let expected =
    "  . 1 .                                              \n"
    ^ "3 = .                                              \n"
    ^ ". . .                                              \n"
    |> _init_state
  in
  assert_equal_grids state expected

let%test_unit "neq_fired" =
  let state =
    "   . 1 .                                               \n"
    ^ " 3 # .                                               \n"
    ^ " . . .                                               \n"
    |> _init_state
    |> step
  in
  let expected =
    "  . . .                                              \n"
    ^ ". # 1                                              \n"
    ^ ". 3 .                                              \n"
    |> _init_state
  in
  assert_equal_grids state expected

let%test_unit "neq_unfired" =
  let state =
    "   . 3 .                                               \n"
    ^ " 3 # .                                               \n"
    ^ " . . .                                               \n"
    |> _init_state
    |> step
  in
  let expected =
    "  . 3 .                                              \n"
    ^ "3 # .                                              \n"
    ^ ". . .                                              \n"
    |> _init_state
  in
  assert_equal_grids state expected

let%test_unit "timewarp does not fire immediately" =
  let state =
    "  . . . . . . .                                      \n"
    ^ ". = 4 > . . .                                      \n"
    ^ ". 4 . 2 @ 1 .                                      \n"
    ^ ". v . . 1 . .                                      \n"
    ^ ". . . . . . .                                      \n"
    ^ "S < . . . . .                                      \n"
    |> _init_state
    |> step
  in
  let expected =
    "  . . . . . . .                                      \n"
    ^ ". = . > 4 . .                                      \n"
    ^ ". . . 2 @ 1 .                                      \n"
    ^ ". v . . 1 . .                                      \n"
    ^ ". 4 . . . . .                                      \n"
    ^ "S < . . . . .                                      \n"
    |> _init_state
  in
  assert_equal_grids state expected

let%test_unit "timewarp pure rewind" =
  let state =
    "  . . . . . . .                                      \n"
    ^ ". = 4 > . . .                                      \n"
    ^ ". 4 . 2 @ 1 .                                      \n"
    ^ ". v . . 1 . .                                      \n"
    ^ ". . . . . . .                                      \n"
    ^ "S < . . . . .                                      \n"
    |> _init_state
    |> step
    |> step
  in
  let expected =
    "  . . . . . . .                                      \n"
    ^ ". = 4 > . . .                                      \n"
    ^ ". 4 . 2 @ 1 .                                      \n"
    ^ ". v . . 1 . .                                      \n"
    ^ ". . . . . . .                                      \n"
    ^ "S < . . . . .                                      \n"
    |> _init_state
  in
  assert_equal_grids state expected

let%test_unit "timewarp pure rewind" =
  let state =
    "  . . . . . . .                                      \n"
    ^ ". = 4 > . . .                                      \n"
    ^ ". 4 . 2 @ 1 .                                      \n"
    ^ ". v . . 1 . .                                      \n"
    ^ ". . . . . . .                                      \n"
    ^ "S < . . . . .                                      \n"
    |> _init_state
    |> step
    |> step
  in
  let expected =
    "  . . . . . . .                                      \n"
    ^ ". = 4 > . . .                                      \n"
    ^ ". 4 . 2 @ 1 .                                      \n"
    ^ ". v . . 1 . .                                      \n"
    ^ ". . . . . . .                                      \n"
    ^ "S < . . . . .                                      \n"
    |> _init_state
  in
  assert_equal_grids state expected

let%test_unit "timewarp pure rewind twice" =
  let state =
    "  . . . . . . .                                      \n"
    ^ ". = 4 > . . .                                      \n"
    ^ ". 4 . 2 @ 1 .                                      \n"
    ^ ". v . . 1 . .                                      \n"
    ^ ". . . . . . .                                      \n"
    ^ "S < . . . . .                                      \n"
    |> _init_state
    |> step
    |> step
    |> step
    |> step
  in
  let expected =
    "  . . . . . . .                                      \n"
    ^ ". = 4 > . . .                                      \n"
    ^ ". 4 . 2 @ 1 .                                      \n"
    ^ ". v . . 1 . .                                      \n"
    ^ ". . . . . . .                                      \n"
    ^ "S < . . . . .                                      \n"
    |> _init_state
  in
  assert_equal_grids state expected
