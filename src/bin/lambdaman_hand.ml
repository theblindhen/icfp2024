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

let hand_solutions =
  [
    [];
    [];
    [];
    [];
    [ (spiral "DLUR" 100 999999, fun _ -> true) ];
    List.map
      (List.append hand_solutions6 (generate_hand_solutions6 ()))
      ~f:(fun x -> (x, validate_result6));
    [];
    List.map hand_solutions8 ~f:(fun s ->
        ( s,
          fun s ->
            printf "%s\n" s;
            true ));
    List.map hand_solutions9 ~f:(fun s -> (s, fun _ -> true));
    [];
    [ (spiral "DLUR" 100 999999, fun _ -> true) ];
  ]

let get_hand_solutions level =
  if
    level - 1 < List.length hand_solutions
    && not (List.is_empty (List.nth_exn hand_solutions (level - 1)))
  then Some (List.nth_exn hand_solutions (level - 1))
  else None

let get_random_solutions dir level =
  let filename = dir ^ "/lambdaman" ^ Int.to_string level ^ ".txt" in
  let _grid = Util.read_grid filename in
  None

let check = ref false
let dir = ref ""
let random = ref false
let write = ref false
let submit = ref false

let speclist =
  [
    ("--check", Arg.Set check, "Evaluate and check solutions (default: false)");
    ("--dir", Arg.Set_string dir, "Map directory (default: current directory)");
    ("--random", Arg.Set random, "Use random solutions");
    ("--write", Arg.Set write, "Write solutions to file (default: false)");
    ("--submit", Arg.Set submit, "Submit solutions to server (default: false)");
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
             | String s -> if not (validator s) then printf "XXX Validation failed: %s\n" s
             | _ -> printf "XXX Didn't evaluate to a string literal: %s\n" (Language.deparse res));

          if !write then
            Solutions.write_solution "lambdaman" dir (Int.to_string level)
              (Language.deparse (wrap_prefix level sol));

          if !submit then
            match Communication.request_with_auth (Language.deparse (wrap_prefix level sol)) with
            | Language.String s -> print_endline s
            | _ -> print_endline "Failed to submit")
