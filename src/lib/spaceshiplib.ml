(** An optimistic heuristic for the number of time steps to fly from (x,y) to (x',y') *)
let fly_dist (x, y, vx, vy) (x', y') =
  let composant_dist (z, vz) z' =
    let dz = z' - z in
    let nonneg a = if a < 0 then -1 else 1 in
    let vz_signed = nonneg dz * nonneg vz in
    let rec try_t t = if (t * vz_signed) + (t * t / 2) + 1 >= dz then t else try_t (t + 1) in
    try_t 1
  in
  max (composant_dist (x, vx) x') (composant_dist (y, vy) y')

(* TESTS *)
let%test_unit "fly_dist" =
  let v = fly_dist (0, 0, 0, 0) (0, 0) in
  assert (v = 0)

let%test_unit "fly_dist2" =
  let v = fly_dist (0, 0, 0, 0) (0, 0) in
  assert (v = 0)
