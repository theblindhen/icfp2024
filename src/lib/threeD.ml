open Core

type binop = ShiftLeft | ShiftRight | Plus | Minus | Equal | NotEqual | Mult | Div | Mod

let binop_to_string = function
  | ShiftLeft -> "<"
  | ShiftRight -> ">"
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

type cell =
  | (* empty *) Empty
  | Literal of Bigint.t
  | Binop of binop
  | Place of placeholder
  | Timewarp

let cell_to_string = function
  | Empty -> "."
  | Literal i -> Bigint.to_string_hum i
  | Binop b -> binop_to_string b
  | Place p -> placeholder_to_string p
  | Timewarp -> "@"

type grid = cell array array

let grid_to_string (grid : grid) =
  (* First map each cell to its string *)
  let grid_strs = Array.map grid ~f:(fun row -> Array.map row ~f:cell_to_string) in
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

type state = {
  grid : grid;
  (* history : grid list; *)
  offset : int * int;
  maxdims : int * int;
  current_time : int;
  current_ticks : int;
}

let parse_grid (rows : string array) : grid =
  let grid =
    Array.map rows ~f:(fun row ->
        let tokens = String.split ~on:' ' row in
        List.map tokens ~f:(fun token ->
            match token with
            | "." -> Empty
            | "@" -> Timewarp
            | "<" -> Binop ShiftLeft
            | ">" -> Binop ShiftRight
            | "+" -> Binop Plus
            | "-" -> Binop Minus
            | "=" -> Binop Equal
            | "#" -> Binop NotEqual
            | "*" -> Binop Mult
            | "/" -> Binop Div
            | "%" -> Binop Mod
            | "A" -> Place A
            | "B" -> Place B
            | "S" -> Place S
            | x -> (
                (* try to get an int *)
                try Literal (Bigint.of_string x) with
                | _ -> failwith ("Invalid token: " ^ x)))
        |> Array.of_list)
  in
  (* assert all rows are equal length *)
  let row_length = Array.length grid.(0) in
  if not (Array.for_all grid ~f:(fun row -> Array.length row = row_length)) then
    failwith "All rows must be the same length";
  grid

let init_state (rows : string array) =
  let grid = parse_grid rows in
  let offset = (0, 0) in
  let maxdims = (Array.length grid, Array.length grid.(0)) in
  let current_time = 0 in
  let current_ticks = 0 in
  { grid; offset; maxdims; current_time; current_ticks }
