(** This module provides types and operations for handling
   consistencies. A consitency is a property obtained after a
   filtering operation f(s,p): given an abstract value s, and a
   predicate p, it computes a set s' \subseteq s such that : \forall x
   \in s, p(x) \implies x \in s' *)

type 'a t =
  | Sat                   (* when s' = s *)
  | Unsat                 (* when s' = \emptyset *)
  | Filtered of 'a * bool (* filtered (s',b) : \forall x \in s', b \implies p(x) *)

let map f = function
   | Unsat -> Unsat
   | Sat -> Sat
   | Filtered (a,b) -> Filtered (f a,b)

let bind f = function
   | Unsat -> Unsat
   | Sat -> Sat
   | Filtered (a,b) -> f a b

let print fmt = function
   | Unsat -> Format.fprintf fmt "Unsat"
   | Sat -> Format.fprintf fmt "Sat"
   | Filtered (_,true) -> Format.fprintf fmt "filtered successfully"
   | Filtered (_,false) -> Format.fprintf fmt "filtered"
