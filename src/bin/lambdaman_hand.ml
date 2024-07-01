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
      let_op rec_op (fun rec_op ->
          app
            (app rec_op
               (abs (fun r ->
                    abs (fun i ->
                        if_op (eq_op i (Integer n)) (String "")
                          (concat_op
                             (app
                                (app rec_op Lambdaman_pack.decode_dirs_body_safe)
                                (mult_op seed (add_op seed i)))
                             (app r (add_op i (Integer (big 1)))))))))
            (Integer (big 0))))

let random_mover_linear_cong double n =
  let a = Bigint.of_int 1664543 in
  let c = Bigint.of_int 1013904223 in
  abs (fun seed ->
      let_op rec_op (fun rec_op ->
          app
            (app
               (app rec_op
                  (abs (fun r ->
                       abs (fun i ->
                           abs (fun rnd ->
                               if_op (eq_op i (Integer n)) (String "")
                                 (concat_op
                                    (app
                                       (app rec_op
                                          (if double then
                                             Lambdaman_pack.decode_dirs_body_safe_double
                                           else Lambdaman_pack.decode_dirs_body_safe))
                                       rnd)
                                    (app
                                       (app r (add_op i (Integer (big 1))))
                                       (mod_op (add_op (mult_op (Integer a) rnd) (Integer c)) seed))))))))
               (Integer (big 0)))
            seed))

(* let double =
   app rec_op
     (abs (fun r ->
          abs (fun s ->
              if_op (eq_op s (String "")) (String "")
                (let_op
                   (take_op (Integer (big 1)) s)
                   (fun c -> concat_op (concat_op c c) (app r (drop_op (Integer (big 1)) s))))))) *)

let use_random_move_seeds level seeds =
  let time = 1_000_000 in
  let seed_len = 25 in
  let doubling = level > 10 && level < 17 in
  let pr_seed_time = time / 8 in
  let ilen = if doubling then pr_seed_time / 2 else pr_seed_time in
  let reps = ilen / seed_len in
  let seeds = List.map ~f:(fun s -> Integer (Bigint.of_string s)) seeds in
  let path =
    let_op
      (random_mover_linear_cong doubling (big reps))
      (fun path_gen ->
        List.fold seeds ~init:(String "") ~f:(fun acc seed -> concat_op acc (app path_gen seed)))
  in
  path

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
      ( app Lambdaman_pack.decode_dirs
          (Integer
             (Lambdaman_pack.encode_dirs
                "RDLLLULURRULRRRRRDLRRDLRRDLLDRLLDRLLLLLLLULRRLULLURULRURURURRRRRRRRRULRRDDLLLRDRRDLRDDDLDLRRDDLULDLUDLLLLLLLLLULDLLURURLLURLUUUURULRRULLURRRRLD")),
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
    [
      ( use_random_move_seeds 11
          [
            "310320896106735";
            "994417900922617";
            "434070468657171";
            "265454041144311";
            "518567628587229";
            "1060388247021576";
            "231532122788503";
            "838371804846803";
          ],
        fun _ -> true );
    ];
    (* lvl 12 *)
    [
      ( use_random_move_seeds 12
          [
            "1020701028292982";
            "359531091385203";
            "616379947361168";
            "944605048394827";
            "410468893190413";
            "948626125603513";
            "562238820118079";
          ],
        fun _ -> true );
    ];
    (* lvl 13 *)
    [
      ( use_random_move_seeds 13
          [
            "1098980852083101";
            "467459250654953";
            "834783854302949";
            "810039608059304";
            "124036403185730";
            "160818393126735";
            "1045534844990799";
          ],
        fun _ -> true );
    ];
    (* lvl 14 *)
    [
      ( use_random_move_seeds 14
          [
            "253414150635291";
            "1053935229303375";
            "457433119265799";
            "484541520435701";
            "995120241016045";
            "372613064468116";
            "715088766622569";
          ],
        fun _ -> true );
    ];
    (* lvl 15 *)
    [
      ( use_random_move_seeds 15
          [
            "65127901094417";
            "731042368727083";
            "823644328558372";
            "1072016859587940";
            "358339787414299";
            "223103441056256";
            "459882053407730";
            "698232268627085";
          ],
        fun _ -> true );
    ];
    (* lvl 16 *)
    [
      ( use_random_move_seeds 16
          [
            "824749631174743";
            "33651234577867";
            "410734819134391";
            "1035943323379897";
            "986034489573357";
            "1090894023747339";
            "967950491898109";
          ],
        fun _ -> true );
    ];
    (* lvl 17 *)
    [
      ( use_random_move_seeds 17 [ "639448696149012"; "1017704350826687"; "291020040003992" ],
        fun _ -> true );
    ];
    (* lvl 18 *)
    [
      ( use_random_move_seeds 18
          [
            "839930458749268";
            "791619070878121";
            "1113708197422120";
            "228483696748394";
            "1070376576397760";
            "963493740907283";
            "853374563745149";
            "1013533727118392";
          ],
        fun _ -> true );
    ];
    (* lvl 19 *)
    [];
    (* lvl 20 *)
    [];
    (* lvl 21 *)
    [
      ( take_op
          (Integer (big 999_999))
          (use_random_move_seeds 21
             [
               "771829853253192";
               "905517513183933";
               "941605617732315";
               "835094516548936";
               "809745652409618";
               "206684455127905";
               "813399173201413";
               "902380854217537";
               "829212037734945";
             ]),
        fun _ -> true );
    ];
  ]

let get_hand_solutions level =
  if
    level - 1 < List.length hand_solutions
    && not (List.is_empty (List.nth_exn hand_solutions (level - 1)))
  then Some (List.nth_exn hand_solutions (level - 1))
  else None

exception FoundSolution of Bigint.t list * string * int

let get_random_solutions dir level seed_len windowed eager eager_factor =
  let filename = dir ^ "/lambdaman" ^ Int.to_string level ^ ".txt" in
  let grid = Util.load_char_grid filename in
  let trials = 250 in
  let time = 1_000_000 in
  let doubling = level > 10 && level < 17 in
  let _boost_max = 3 in
  let _boost_prob = 0.8 in
  let seed_generator () = Muttleyman.random_seed seed_len in
  let path_generator len seed =
    let ilen = if doubling then len / 2 else len in
    Muttleyman.pseudo_repeat_random_linear_cong seed_len seed ilen
    |> if doubling then Muttleyman.double_path else Fn.id
    (* Muttleyman.random_moves ilen |> if doubling then Muttleyman.double_path else Fn.id *)
    (* |> Muttleyman.boost_path boost_max boost_prob *)
  in
  try
    (if windowed = 1 && not eager then (
       printf "Trying to find solution at seed len %d%!\n" seed_len;
       for i = 1 to trials do
         printf ".%!";
         let state = Lambdaman_sim.init_state (Array.copy_matrix grid) in
         let seed = seed_generator () in
         let path = path_generator time seed in
         let path =
           if String.length path > time then printf "WARNING: path too long, truncating\n";
           String.prefix path time
         in
         Lambdaman_sim.run_str state path;
         if state.pills |> Hash_set.Poly.is_empty then
           raise (FoundSolution ([ seed ], path, state.ticks))
         else if i = trials then (
           printf "No solution found\nFinal state of last run:\n%s\n"
             (Lambdaman_sim.dump_state state);
           printf "Really, there was no solution\n")
       done)
     else
       let state = Lambdaman_sim.init_state (Array.copy_matrix grid) in
       printf "Lambdaman in (%d, %d)\n" (fst state.lambdaman) (snd state.lambdaman);
       if eager then (
         let path_generator = path_generator (time / eager_factor) in
         printf "Eager punter going for a punt with seed len %d and eager factor %d %!\n" seed_len
           eager_factor;
         match Muttleyman.eager_punter state seed_generator path_generator time trials with
         | None -> ()
         | Some seeds ->
             printf "I think I found a solution!";
             printf "Random seeds:\n%s\n"
               (seeds |> List.map ~f:Bigint.to_string |> String.concat ~sep:" --- ");
             let path = seeds |> List.map ~f:path_generator |> String.concat in
             let state = Lambdaman_sim.init_state (Array.copy_matrix grid) in
             Lambdaman_sim.run_str state path;
             if state.pills |> Hash_set.Poly.is_empty && state.ticks < time then
               raise (FoundSolution (seeds, path, state.ticks))
             else printf "The solution was a lie!\nFinal Ticks: %d" state.ticks
         (* printf "The solution was a lie!\nFinal state of last run:\n%s\n"
            (Lambdaman_sim.dump_state state)) *))
       else
         let path_generator = path_generator (time / windowed) in
         printf "Going for a windowed solution with seed len %d%!\n" seed_len;
         match Muttleyman.window_seeder windowed state seed_generator path_generator trials with
         | None -> ()
         | Some seeds ->
             printf "I think I found a solution!";
             printf "Random seeds:\n[%s]\n"
               (seeds |> List.map ~f:Bigint.to_string |> String.concat ~sep:"; ");
             let path = seeds |> List.map ~f:path_generator |> String.concat in
             let state = Lambdaman_sim.init_state (Array.copy_matrix grid) in
             Lambdaman_sim.run_str state path;
             if state.pills |> Hash_set.Poly.is_empty then
               raise (FoundSolution (seeds, path, state.ticks))
             else
               printf "The solution was a lie!\nFinal state of last run:\n%s\n"
                 (Lambdaman_sim.dump_state state));
    None
  with
  | FoundSolution (seeds, _path, ticks) ->
      printf "FOUND SOLUTION:\n";
      (* printf "path:\n%s\n" path; *)
      printf "Eager factor: %d" eager_factor;
      printf "Random seeds:\n[%s]\n"
        (seeds
        |> List.map ~f:Bigint.to_string
        |> List.map ~f:(fun n -> "\"" ^ n ^ "\"")
        |> String.concat ~sep:"; ");
      printf "Level %d: Ticks produced: %d -- %d seeds %s\n" level ticks (List.length seeds)
        (if ticks > 1_000_000 then " (Obs: Needs drop!)" else "");
      Some []

let check = ref false
let dir = ref ""
let random = ref false
let write = ref false
let submit = ref false
let sim = ref false
let windowed = ref 1
let eager = ref false
let seed_len = ref 25
let eager_factor = ref 8

let speclist =
  [
    ("--check", Arg.Set check, "Evaluate and check solutions (default: false)");
    ("--dir", Arg.Set_string dir, "Map directory (default: current directory)");
    ("--random", Arg.Set random, "Use random solutions");
    ("--write", Arg.Set write, "Write solutions to file (default: false)");
    ("--submit", Arg.Set submit, "Submit solutions to server (default: false)");
    ("--sim", Arg.Set sim, "Simulate submission (default: false)");
    ("--windowed", Arg.Set_int windowed, "The number of windows for random (default: 1)");
    ("--eager", Arg.Set eager, "Use the eager punter for random (default: false)");
    ( "--eager_factor",
      Arg.Set_int eager_factor,
      "Use this eager factor for eager random (default: 8)" );
    ("--seed_len", Arg.Set_int seed_len, "Set the seed length for random (default: 25)");
  ]

let usage_msg = "lambdaman_hand [--check] [--write] [--submit] [--dir <dir>] <level>"
let level = ref 0
let anon_fun l = level := Int.of_string l

(* Main function *)
let () =
  Arg.parse speclist anon_fun usage_msg;
  let dir, level, use_random = (!dir, !level, !random) in
  let sols =
    if use_random then get_random_solutions dir level !seed_len !windowed !eager !eager_factor
    else get_hand_solutions level
  in
  match sols with
  | None -> printf "No solution for level %d\n" level
  | Some sols ->
      List.iter sols ~f:(fun (sol, validator) ->
          let icfp = Language.deparse sol in
          printf "%5d: %s\n%!" (String.length icfp) icfp;
          printf "%5d: %s\n%!" (String.length icfp) (Language.pp_as_lambda 50 sol);
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
