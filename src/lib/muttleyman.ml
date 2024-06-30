open Core
open Util
open Lambdaman_sim

let decode_dir = function
  | 0 -> 'L'
  | 1 -> 'D'
  | 2 -> 'U'
  | 3 -> 'R'
  | _ -> failwith "Invalid move"

let encode_path s =
  String.to_list s
  |> List.map ~f:Lambdaman_pack.encode_dir
  |> List.fold ~init:Bigint.zero ~f:(fun acc x -> Bigint.((acc * big 4) + x))

let decode_path n =
  let rec aux acc n =
    if Bigint.(n = zero) then acc
    else
      let q, r = quo_rem n (big 4) in
      aux (decode_dir (small r) :: acc) q
  in
  aux [] n |> List.rev |> String.of_char_list

(** Completely random moves *)
let random_moves n = String.init n ~f:(fun _ -> "RULD".[Random.int (String.length "RULD")])

let random_seed seed_len = Bigint.random Bigint.(big 4 ** big seed_len)

(** Repeat the input N times *)
let repeat s n = String.concat (List.init n ~f:(fun _ -> s))

let repeat_random seed_len total =
  let n = (total / seed_len) + 1 in
  repeat (random_moves seed_len) n

let pseudo_repeat_random seed_len seed total =
  (* Number of repetitions *)
  let reps = total / seed_len in
  let rec aux acc i =
    if i = reps then acc
    else
      let shuffle_seed = Bigint.(seed * (seed + big i)) in
      aux (decode_path shuffle_seed :: acc) (i + 1)
  in
  aux [] 0 |> List.rev |> String.concat

let boost_path boost_max prob path =
  path
  |> String.to_list
  |> List.map ~f:(fun c ->
         if Float.( < ) (Random.float 1.0) prob then
           let reps = Random.int boost_max in
           repeat (String.of_char c) reps
         else String.of_char c)
  |> String.concat

let double_path path =
  path |> String.to_list |> List.map ~f:(fun c -> repeat (String.of_char c) 2) |> String.concat

type window = (int * int) * (int * int)

let get_windows state =
  (* Get quadrants *)
  let grid = state.grid in
  let x_mid = Array.length grid.(0) / 2 in
  let y_mid = Array.length grid / 2 in
  let x_max, y_max = (Array.length grid.(0), Array.length grid) in
  let windows =
    [
      ((0, 0), (x_mid, y_mid));
      ((x_mid, 0), (x_max, y_mid));
      ((0, y_mid), (x_mid, y_max));
      ((x_mid, y_mid), (x_max, y_max));
    ]
  in
  (* Order the windows so that the one with lambdaman in is first *)
  let lambdaman = state.lambdaman in
  List.sort windows ~compare:(fun ((x1, y1), (x2, y2)) _ ->
      (if x1 <= fst lambdaman && x2 > fst lambdaman then -1 else 0)
      + if y1 <= snd lambdaman && y2 > snd lambdaman then -1 else 0)

exception FoundSolution of (state * Bigint.t)

let find_good_seed window init_state seed_generator generator trials =
  (* At the state we're currently at, try to find a good seed for the generator
   * which results in the window being emptied of pills. *)
  let (x1, y1), (x2, y2) = window in
  printf "Finding seed for window (%d, %d) to (%d, %d)\n%!" x1 y1 x2 y2;
  printf "State\n%s" (dump_state init_state);
  let pills_in_window state =
    state.pills
    |> Hash_set.Poly.filter ~f:(fun (x, y) -> x >= x1 && x < x2 && y >= y1 && y < y2)
    |> Hash_set.Poly.length
  in
  try
    for _ = 1 to trials do
      printf "Lambdaman at %d, %d and %d pills left in window \n%!" (fst init_state.lambdaman)
        (snd init_state.lambdaman) (pills_in_window init_state);
      let seed = seed_generator () in
      let path = generator seed in
      let state = duplicate_state init_state in
      run_str state path;
      if pills_in_window state = 0 then raise (FoundSolution (state, seed))
    done;
    None
  with
  | FoundSolution (state, seed) -> Some (state, seed)

let window_seeder init_state seed_generator generator trials =
  let windows = get_windows init_state in
  let result =
    List.fold windows
      ~init:(Some (init_state, []))
      ~f:(fun res window ->
        match res with
        | None -> None
        | Some (state, seeds) -> (
            match find_good_seed window state seed_generator generator trials with
            | None -> None
            | Some (state', seed) -> Some (state', seed :: seeds)))
  in
  match result with
  | None -> None
  | Some (_state, seeds) -> Some (List.zip_exn windows (List.rev seeds))
