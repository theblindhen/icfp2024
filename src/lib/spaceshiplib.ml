open Core

(** An optimistic heuristic for the number of time steps to fly from (x,y) to (x',y') *)
let fly_dist (x, y, vx, vy) (x', y') =
  let composant_dist (z, vz) z' =
    let dz = abs (z' - z) in
    let is_non_neg a = if a < 0 then -1 else 1 in
    let vz_signed = is_non_neg dz * is_non_neg vz * vz in
    let rec try_t t = if (t * vz_signed) + (t * t / 2) + 1 >= dz then t else try_t (t + 1) in
    try_t 1
  in
  if x = x' && y = y' then 0 else max (composant_dist (x, vx) x') (composant_dist (y, vy) y')

(* TESTS *)
let%test_unit "fly_dist" =
  let t = fly_dist (0, 0, 0, 0) (0, 0) in
  printf "%d\n" t;
  assert (t = 0)

let%test_unit "fly_dist2" =
  let t = fly_dist (-32, -115, -5, -7) (-45, -127) in
  printf "%d\n" t;
  assert (t = 2)
