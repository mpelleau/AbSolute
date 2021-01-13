(* This module implements a 3-valued logic *)

type t = False | True | Unknown

let and_kleene x y =
  match (x, y) with
  | False, _ | _, False -> False
  | Unknown, _ | _, Unknown -> Unknown
  | True, True -> True

let or_kleene x y =
  match (x, y) with
  | True, _ | _, True -> True
  | Unknown, _ | _, Unknown -> Unknown
  | False, False -> False

let not_kleene = function True -> False | False -> True | Unknown -> Unknown

let of_bool = function true -> True | false -> False
