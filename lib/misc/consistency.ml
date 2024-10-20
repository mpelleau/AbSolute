(** This module provides types and operations for handling consistencies. A
    consitency is a property obtained after a filtering operation f(s,p): given
    an abstract value s, and a predicate p, it computes a set {m s' \subseteq s}
    such that : {m \forall x \in s, p(x) \implies x \in s'} *)

type 'a t =
  | Sat  (** when {m s' = s} *)
  | Unsat  (** when {m s' = \emptyset} *)
  | Filtered of 'a * bool
      (** {m filtered (s',b) : \forall x \in s', b \implies p(x)} *)
  | Pruned of {sure: 'a list; unsure: 'a list}

type feasible = Unfeasible | Maybe | Witness of Csp.instance

let print fmt = function
  | Unsat -> Format.fprintf fmt "Unsat"
  | Sat -> Format.fprintf fmt "Sat"
  | Filtered (_, true) -> Format.fprintf fmt "filtered successfully"
  | Filtered (_, false) -> Format.fprintf fmt "filtered"
  | Pruned _ -> Format.fprintf fmt "divided"

let map f = function
  | Unsat -> Unsat
  | Sat -> Sat
  | Filtered (a, b) -> Filtered (f a, b)
  | Pruned {sure; unsure} ->
      Pruned {sure= List.map f sure; unsure= List.map f unsure}

let bind f = function
  | Unsat -> Unsat
  | Sat -> Sat
  | Filtered (a, b) -> f a b
  | Pruned _ -> failwith ""

(* (\** apply several filtering operation, with early termination when a *)
(*     unsatisfiable state is met *\) *)
(* let fold_and f init l = *)
(*   let next ((abs, flag) as acc) c = *)
(*     match f abs c with *)
(*     | Unsat -> raise Exit *)
(*     | Sat -> acc *)
(*     | Filtered (a, sat) -> (a, flag && sat) *)
(*     | Pruned (a, sat) -> (a, flag && sat) *)
(*   in *)
(*   try *)
(*     let a, b = List.fold_left next (init, true) l in *)
(*     Filtered (a, b) *)
(*   with Exit -> Unsat *)
