open Core
open Lib
open Language
open Metalanguage
open Util

let wrap_prefix level term = concat_op (String ("solve lambdaman" ^ Int.to_string level ^ " ")) term

let hand_solutions6 =
  [
    let_op
      (abs (fun a -> concat_op a (concat_op (concat_op a a) (concat_op a a))))
      (fun mult -> app mult (app mult (String "RRRRRRRR")));
    let_op
      (abs (fun a -> concat_op a a))
      (fun double ->
        let_op (String "RRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR") (fun x ->
            app double (app double x)));
    let_op
      (abs (fun a -> concat_op a (concat_op a a)))
      (fun double -> app double (app double (app double (String "RRRRRRRR"))));
    let_op
      (abs (fun a -> concat_op a (concat_op (concat_op a a) (concat_op a a))))
      (fun mult -> app mult (app mult (String "RRRRRRRR")));
  ]

let mults =
  [
    String "Not supported";
    String "Not supported";
    abs (fun a -> concat_op a a);
    abs (fun a -> concat_op a (concat_op a a));
    abs (fun a -> concat_op (concat_op a a) (concat_op a a));
    abs (fun a -> concat_op a (concat_op (concat_op a a) (concat_op a a)));
    abs (fun a -> concat_op (concat_op a a) (concat_op (concat_op a a) (concat_op a a)));
    abs (fun a ->
        concat_op a (concat_op (concat_op a a) (concat_op (concat_op a a) (concat_op a a))));
  ]

let generate_hand_solutions6 () =
  let specs =
    [
      (200, ((2, 3), 25));
      (200, ((2, 2), 50));
      (200, ((5, 2), 8));
      (200, ((5, 1), 40));
      (203, ((7, 1), 29));
      (207, ((3, 2), 23));
      (208, ((2, 4), 13));
      (216, ((3, 3), 8));
      (224, ((2, 5), 7));
      (243, ((3, 5), 1));
      (243, ((3, 4), 3));
      (245, ((7, 2), 5));
      (250, ((5, 3), 2));
      (256, ((2, 8), 1));
      (256, ((2, 7), 2));
      (256, ((2, 6), 4));
      (343, ((7, 3), 1));
      (512, ((2, 9), 1));
      (625, ((5, 4), 1));
      (729, ((3, 6), 1));
    ]
  in
  let rec rep n f x = if n = 0 then x else rep (n - 1) f (app f x) in
  let drop_if size t = if size > 200 then drop_op (Integer (Util.big (size - 200))) t else t in
  List.map specs ~f:(fun (size, ((mult, power), lit_length)) ->
      let mult_op = List.nth_exn mults mult in
      let literal = String (String.make lit_length 'R') in
      if power > 1 then let_op mult_op (fun mult -> drop_if size (rep power mult literal))
      else drop_if size (rep power mult_op literal))

let validate_result6 res = String.is_prefix ~prefix:(String.make 200 'R') res

let spiral chars n max =
  let_op (String chars) (fun cs ->
      app
        (app rec_op
           (abs (fun r ->
                abs (fun x ->
                    if_op
                      (eq_op x (Integer (big max)))
                      (String "")
                      (concat_op
                         (take_op
                            (Integer (big 1))
                            (drop_op
                               (mod_op
                                  (div_op x (Integer (big n)))
                                  (Integer (big (String.length chars))))
                               cs))
                         (app r (add_op x (Integer (big 1)))))))))
        (Integer (big 0)))

let hand_solutions8 = [ spiral "DLUR" 100 500000 ]

let hand_solutions9 =
  [
    (* let rec move x =
       if x = (50 * 50) - 1 then ""
       else (if x % 50 = 49 then "D" else if x % 100 < 50 then "R" else "L") ^ move (x + 1) *)
    app
      (app rec_op
         (abs (fun r ->
              abs (fun x ->
                  if_op
                    (eq_op x (Integer (big ((50 * 50) - 1))))
                    (String "")
                    (concat_op
                       (if_op
                          (eq_op (mod_op x (Integer (big 50))) (Integer (big 49)))
                          (String "D")
                          (if_op
                             (lt_op (mod_op x (Integer (big 100))) (Integer (big 50)))
                             (String "R") (String "L")))
                       (app r (add_op x (Integer (big 1)))))))))
      (Integer (big 0));
  ]

let random_mover n seed =
  let_op (Integer seed) (fun seed ->
      app
        (app rec_op
           (abs (fun r ->
                abs (fun i ->
                    if_op (eq_op i (Integer n)) (String "")
                      (concat_op
                         (app Lambdaman_pack.decode_dirs_safe (mult_op seed (add_op seed i)))
                         (app r (add_op i (Integer (big 1)))))))))
        (Integer (big 0)))

let hand_solutions =
  [
    (* lvl 1 *)
    [];
    (* lvl 2 *)
    [];
    (* lvl 3 *)
    [];
    (* lvl 4 *)
    [ (random_mover (big 70000) (Bigint.of_string "456"), fun _ -> true) ];
    (* lvl 5 *)
    [
      ( String
          "RDLLLULURRULRRRRRDLRRDLRRDLLDRLLDRLLLLLLLULRRLULLURULRURURURRRRRRRRRULRRDDLLLRDRRDLRDDDLDLRRDDLULDLUDLLLLLLLLLULDLLURURLLURLUUUURULRRULLURRRRLD",
        fun _ -> true );
    ];
    (* lvl 6 *)
    List.map
      (List.append hand_solutions6 (generate_hand_solutions6 ()))
      ~f:(fun x -> (x, validate_result6));
    (* lvl 7 *)
    [
      ( random_mover (big 5000)
          (Bigint.of_string "11376319153570422810653774824059613450159648771649"),
        fun _ -> true );
    ];
    (* lvl 8 *)
    List.map hand_solutions8 ~f:(fun s ->
        ( s,
          fun s ->
            printf "%s\n" s;
            true ));
    (* lvl 9 *)
    List.map hand_solutions9 ~f:(fun s -> (s, fun _ -> true));
    (* lvl 10 *)
    [ (random_mover (big 48000) (Bigint.of_string "419253"), fun _ -> true) ];
    (* lvl 11 *)
    [ (spiral "DLUR" 100 999999, fun _ -> true) ];
  ]

let get_hand_solutions level =
  if
    level - 1 < List.length hand_solutions
    && not (List.is_empty (List.nth_exn hand_solutions (level - 1)))
  then Some (List.nth_exn hand_solutions (level - 1))
  else None

exception FoundSolution of Bigint.t * string * int

let get_random_solutions dir level =
  let filename = dir ^ "/lambdaman" ^ Int.to_string level ^ ".txt" in
  let grid = Util.load_char_grid filename in
  let seed_len = 100 in
  let trials = 100 in
  let doubling = level > 10 && level < 17 in
  let boost_max = 3 in
  let boost_prob = 0.8 in
  let seed_generator () = Muttleyman.random_seed seed_len in
  let path_generator _seed =
    Muttleyman.random_moves (1_000_000 / boost_max * 2)
    |> (if doubling then Muttleyman.double_path else Fn.id)
    |> Muttleyman.boost_path boost_max boost_prob
  in
  try
    printf "Trying to find solution at seed len %d%!" seed_len;
    for i = 1 to trials do
      printf ".%!";
      let state = Lambdaman_sim.init_state (Array.copy_matrix grid) in
      let seed = seed_generator () in
      let path = path_generator seed in
      let path =
        if String.length path > 1_000_000 then printf "WARNING: path too long, truncating\n";
        String.prefix path 1_000_000
      in
      Lambdaman_sim.run_str state path;
      if state.pills |> Hash_set.Poly.is_empty then
        let rounds = (state.ticks / (seed_len * 2)) + 1 in
        raise (FoundSolution (seed, path, rounds))
      else if i = trials then (
        printf "No solution found\nFinal state of last run:\n%s\n" (Lambdaman_sim.dump_state state);
        printf "Really, there was no solution\n")
    done;
    None
  with
  | FoundSolution (seed, _path, rounds) ->
      printf "FOUND SOLUTION:\n";
      (* printf "path:\n%s\n" path; *)
      printf "Random seed:\n%s\n" (Bigint.to_string seed);
      printf "Rounds needed: %d\n" rounds;
      None

let check = ref false
let dir = ref ""
let random = ref false
let write = ref false
let submit = ref false
let sim = ref false

let speclist =
  [
    ("--check", Arg.Set check, "Evaluate and check solutions (default: false)");
    ("--dir", Arg.Set_string dir, "Map directory (default: current directory)");
    ("--random", Arg.Set random, "Use random solutions");
    ("--write", Arg.Set write, "Write solutions to file (default: false)");
    ("--submit", Arg.Set submit, "Submit solutions to server (default: false)");
    ("--sim", Arg.Set sim, "Simulate submission (default: false)");
  ]

let usage_msg = "lambdaman_hand [--check] [--write] [--submit] [--dir <dir>] <level>"
let level = ref 0
let anon_fun l = level := Int.of_string l

(* Main function *)
let () =
  Arg.parse speclist anon_fun usage_msg;
  let dir, level, use_random = (!dir, !level, !random) in
  let sols = if use_random then get_random_solutions dir level else get_hand_solutions level in
  match sols with
  | None -> printf "No solution for level %d\n" level
  | Some sols ->
      List.iter sols ~f:(fun (sol, validator) ->
          let icfp = Language.deparse sol in
          printf "%5d: %s\n%!" (String.length icfp) icfp;
          (if !check then
             let res = Interpreter.eval sol in
             match res with
             | String s ->
                 printf "Result: %s\n" s;
                 if not (validator s) then printf "XXX Validation failed: %s\n" s
             | _ -> printf "XXX Didn't evaluate to a string literal: %s\n" (Language.deparse res));

          if !write then
            Solutions.write_solution "lambdaman" dir (Int.to_string level)
              (Language.deparse (wrap_prefix level sol));

          if !submit then
            match Communication.request_with_auth (Language.deparse (wrap_prefix level sol)) with
            | Language.String s -> print_endline s
            | _ -> print_endline "Failed to submit")
