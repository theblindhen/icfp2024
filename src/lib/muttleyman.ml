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

let pseudo_repeat_random_linear_cong seed_len seed total =
  (* Number of repetitions *)
  let a = Bigint.of_int 1664543 in
  let c = Bigint.of_int 1013904223 in
  let reps = total / seed_len in
  let rec aux acc rnd i =
    if i = reps then acc
    else
      let num = rnd in
      let rnd = Bigint.(((a * rnd) + c) % seed) in
      aux (decode_path num :: acc) rnd (i + 1)
  in
  aux [] seed 0 |> List.rev |> String.concat

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

let get_windows n_windows state =
  let (xmin, ymin), (xmax, ymax) =
    ((0, 0), (Array.length state.grid.(0), Array.length state.grid))
  in
  let quadrants (xmin, ymin) (xmax, ymax) =
    let xmid = (xmin + xmax) / 2 in
    let ymid = (ymin + ymax) / 2 in
    [
      ((xmin, ymin), (xmid, ymid));
      ((xmid, ymin), (xmax, ymid));
      ((xmin, ymid), (xmid, ymax));
      ((xmid, ymid), (xmax, ymax));
    ]
  in
  match n_windows with
  | 4 -> quadrants (xmin, ymin) (xmax, ymax)
  | 8 ->
      let xmid = (xmin + xmax) / 2 in
      let ymid = (ymin + ymax) / 2 in
      quadrants (xmin, ymin) (xmid, ymid)
      @ quadrants (xmid, ymin) (xmax, ymid)
      @ quadrants (xmin, ymid) (xmid, ymax)
      @ quadrants (xmid, ymid) (xmax, ymax)
  | _ -> failwith "Unsupported number of windows"

let window_distance (x, y) ((x1, y1), (x2, y2)) =
  (* Manhattan distance from point to window *)
  let dx = if x < x1 then x1 - x else if x >= x2 then x - x2 + 1 else 0 in
  let dy = if y < y1 then y1 - y else if y >= y2 then y - y2 + 1 else 0 in
  dx + dy

exception FoundSolution of (state * Bigint.t)

let pills_in_window ((x1, y1), (x2, y2)) state =
  state.pills
  |> Hash_set.Poly.filter ~f:(fun (x, y) -> x >= x1 && x < x2 && y >= y1 && y < y2)
  |> Hash_set.Poly.length

let run_seed init_state seed_generator generator =
  let seed = seed_generator () in
  let path = generator seed in
  let state = duplicate_state init_state in
  run_str state path;
  (state, seed)

let find_good_seed window init_state seed_generator generator =
  (* At the state we're currently at, try to find a good seed for the generator
   * which results in the window being emptied of pills. *)
  try
    let state, seed = run_seed init_state seed_generator generator in
    if pills_in_window window state = 0 then raise (FoundSolution (state, seed));
    None
  with
  | FoundSolution (state, seed) -> Some (state, seed)

let window_seeder n_windows init_state seed_generator generator trials =
  let windows = get_windows n_windows init_state in
  let rec aux (state, acc_seeds) windows =
    match windows with
    | [] -> Some (List.rev acc_seeds)
    | windows -> (
        let window, rest =
          let distances = List.map windows ~f:(window_distance state.lambdaman) in
          let min_dist = List.min_elt distances ~compare:Int.compare |> Option.value_exn in
          let cur_window =
            List.find_exn windows ~f:(fun w -> window_distance state.lambdaman w = min_dist)
          in
          let rest = List.filter windows ~f:(fun w -> Stdlib.(w <> cur_window)) in
          (cur_window, rest)
        in
        printf "Finding seed for window (%d, %d) to (%d, %d)\n%!"
          (fst (fst window))
          (snd (fst window))
          (fst (snd window))
          (snd (snd window));
        printf "State\n%s" (dump_state state);
        printf "Lambdaman at %d, %d and %d pills left in window \n%!" (fst state.lambdaman)
          (snd state.lambdaman) (pills_in_window window state);
        (* Find 5 Some seeds for the window *)
        let reso =
          Seq.init trials (fun i ->
              if Int.(i % 10 = 0) then printf "Trial %d\n%!" i;
              match find_good_seed window state seed_generator generator with
              | None -> None
              | Some (state', seed) ->
                  printf "Found seed %s\n%!" (Bigint.to_string seed);
                  Some (state', seed))
          |> Seq.filter_map Fn.id
          |> Seq.take 1
          |> Stdlib.List.of_seq
          |> List.sort ~compare:(fun (st1, _) (st2, _) ->
                 Int.compare (Hash_set.Poly.length st1.pills) (Hash_set.Poly.length st2.pills))
          |> List.hd
        in
        match reso with
        | None -> None
        | Some (state', seed) -> aux (state', seed :: acc_seeds) rest)
  in
  match aux (init_state, []) windows with
  | None -> None
  | Some seeds -> Some seeds

let eager_punter init_state seed_generator generator total_time trials_per_punt =
  let rec aux (state, acc_seeds) =
    (* printf "State\n%s" (dump_state state);
       printf "Lambdaman at %d, %d and %d pills left in window \n%!" (fst state.lambdaman)
         (snd state.lambdaman) (Hash_set.length state.pills); *)
    let reso =
      List.init trials_per_punt ~f:(fun _i ->
          (* if Int.(i % 10 = 0) then printf "Trial %d\n%!" (trials_per_punt - i); *)
          run_seed state seed_generator generator)
      |> List.sort ~compare:(fun (st1, _) (st2, _) ->
             Int.compare (Hash_set.Poly.length st1.pills) (Hash_set.Poly.length st2.pills))
      |> List.hd
    in
    match reso with
    | None -> None
    | Some (state', seed) ->
        let pills_left = Hash_set.Poly.length state'.pills in
        printf "Found seed %s (%d pills left)\n%!" (Bigint.to_string seed) pills_left;
        if pills_left = 0 then Some (List.rev (seed :: acc_seeds))
        else if state'.ticks > total_time then (
          printf "Ticks exceeded, bailing\n";
          None)
        else aux (state', seed :: acc_seeds)
  in
  match aux (init_state, []) with
  | None -> None
  | Some seeds -> Some seeds
