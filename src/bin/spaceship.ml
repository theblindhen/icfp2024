open Core

(*
From the task description:

The game operates on an infinite 2D chess board, with the spaceship initially
located on (0,0). The spaceship has a velocity vx and vy, which are initially
both set to 0. In each turn the player can increase/decrease each of those
numbers by at most one, and then the piece moves vx steps to the right and vy
steps up.

Moves are represented with a single digit, inspired by the old numeric pad on a
computer keyboard that we used to have in the old days on Earth. For example, 7
means decreasing vx and increasing vy by 1, while 6 means increasing vx by 1
and keeping vy the same. A path can then be represented by a sequence of
digits, e.g. the path 236659 visits, in this order, the following squares:
  (0,0) (0,-1) (1,-3) (3,-5) (6,-7) (9,-9) (13,-10).

Now the challenge is the following: given a list of squares to be visited, find
a sequence of moves that visits all those squares.
 *)

let () =
  let map_file =
    match Sys.get_argv () with
    | [| _; filename |] -> filename
    | _ -> failwith "Usage: spaceship MAP_FILE"
  in
  let problem =
    In_channel.read_lines map_file
    |> List.filter ~f:(String.( <> ) "") (* Skip empty lines *)
    |> List.map ~f:(fun line ->
           match String.split line ~on:' ' with
           | [ key; value ] -> (Int.of_string key, Int.of_string value)
           | _ -> failwith ("Invalid line: " ^ line))
  in
  (* Simplest possible solution strategy:
      - While where are squares to visit:
        - Pick the next square (later: the closest square)
        - Move towards it with both vx and vy between -1 and 1 (later: larger values)
        - Stop when we reach it (later: keep moving)
  *)
  let _, _, sol_rev =
    List.fold problem ~init:(0, 0, []) ~f:(fun (startx, starty, sol_rev) (endx, endy) ->
        (* Numeric keypad, for reference:
            7 8 9
            4 5 6
            1 2 3 *)
        let moves_x_axis =
          if startx < endx then ('6' :: List.init (endx - startx - 1) ~f:(const '5')) @ [ '4' ]
          else if startx > endx then ('4' :: List.init (startx - endx - 1) ~f:(const '5')) @ [ '6' ]
          else []
        in
        let moves_y_axis =
          if starty < endy then ('8' :: List.init (endy - starty - 1) ~f:(const '5')) @ [ '2' ]
          else if starty > endy then ('2' :: List.init (starty - endy - 1) ~f:(const '5')) @ [ '8' ]
          else []
        in
        (endx, endy, (moves_x_axis @ moves_y_axis) :: sol_rev))
  in
  let sol = List.rev sol_rev |> List.concat in
  print_endline (String.of_char_list sol);
  ()
