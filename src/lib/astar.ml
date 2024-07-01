type 'a problem = {
  move_cost : 'a -> 'a -> int; (* Will only be called on neighboring states *)
  is_goal : 'a -> bool;
  get_next_states : 'a -> 'a list;
  heuristic_cost : 'a -> int;
}

module Astar : sig
  type 'a t = 'a problem

  val search : 'a t -> 'a -> 'a list
  (** [search problem start] returns a path (a list of states) from [start] to
      [problem.goal]. The path minimizes the sum of [problem.move_cost]. *)
end = struct
  type 'a t = 'a problem

  type 'a path = {
    cost_from_start : int;  (** the cost from the start to [head]. *)
    total_cost : int;  (** the total heuristic cost from the start to the goal. *)
    head : 'a;
    tail : 'a list;
  }

  let create_path ?from problem state =
    let cost_from_start, tail =
      match from with
      | None -> (0, [])
      | Some p -> (p.cost_from_start + problem.move_cost p.head state, p.head :: p.tail)
    in
    let total_cost = cost_from_start + problem.heuristic_cost state in
    { cost_from_start; total_cost; tail; head = state }

  (** [better p q] returns [true] if path [p] is better than path [q]. *)
  let better p q = p.total_cost < q.total_cost

  (** [pickup_eq_path p l] returns [Some (q, l')] where [q] is the path that
      indicates the same position as [p] and [l] is a list excluding [q]. *)
  let pickup_eq_path p l =
    match List.partition (fun q -> p.head = q.head) l with
    | [], _ -> None
    | [ q ], l' -> Some (q, l')
    | _ -> failwith "duplicated paths in open/close list"

  (** [trace_next_states problem open_list close_list path] traces the next
      states of [path.head].
      @return [(open_list', close_list')] where [open_list'] and [close_list']
      are respectively an open list and a close list after all of the next
      states are traced. *)
  let trace_next_states problem ol0 cl0 path0 =
    let trace_state (ol, cl) state =
      let path = create_path ~from:path0 problem state in
      match pickup_eq_path path ol with
      | Some (q, ol') -> if better path q then (path :: ol', cl) else (ol, cl)
      | None -> (
          match pickup_eq_path path cl with
          | Some (q, cl') -> if better path q then (path :: ol, cl') else (ol, cl)
          | None -> (path :: ol, cl))
    in
    List.fold_left trace_state (ol0, cl0) (problem.get_next_states path0.head)

  (** [pickup_best_path l] returns [Some (p, l')] where [p] is the path that
      has the least cost in [l] and [l'] is an open list without [p]. *)
  let pickup_best_path = function
    | [] -> None
    | h :: t ->
        let aux (y, l) x = if better y x then (y, x :: l) else (x, y :: l) in
        Some (List.fold_left aux (h, []) t)

  let search problem start =
    let rec aux (ol, cl) =
      match pickup_best_path ol with
      | None -> None (* No path reaches to [problem.goal] *)
      | Some (p, ol') ->
          if problem.is_goal p.head then Some p (* reached to the goal *)
          else aux (trace_next_states problem ol' (p :: cl) p)
    in
    match aux ([ create_path problem start ], []) with
    | None -> raise Not_found
    | Some p -> p.head :: p.tail
end
