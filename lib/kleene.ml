(** This module implements a 3-valued logic *)

(** the type of truth values in a 3vl *)
type t = False | True | Unknown

(** Conjunction *)
let and_kleene x y =
  match (x, y) with
  | False, _ | _, False -> False
  | Unknown, _ | _, Unknown -> Unknown
  | True, True -> True

(** Disjunction *)
let or_kleene x y =
  match (x, y) with
  | True, _ | _, True -> True
  | Unknown, _ | _, Unknown -> Unknown
  | False, False -> False

(** Negation *)
let not_kleene = function True -> False | False -> True | Unknown -> Unknown

(** boolean constructor *)
let of_bool = function true -> True | false -> False
