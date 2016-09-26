(**********************************************************************************)
(*            Module for Abstract Domains for Constraint Programming.             *)
(*   These are abstract domains with consistency, split and precision operators.  *)
(**********************************************************************************)
open Csp

module type AbstractCP = sig

  (*** TYPES ***)
  (* abstract elements *)
  type t

  (*** INSTANCIATION ***)

  (* returns an empty element *)
  val empty : t

  (* adds an unconstrained variable to the environnement *)
  val add_var : t -> typ * var -> t

  (*** PREDICATES ***)

  (* tests if an abstract element is too small to be cut *)
  val is_small : t -> bool

  val is_bottom : t -> bool

  val is_enumerated : t -> bool

  (*** OPERATIONS ***)
  val join: t -> t -> t

  (* pruning *)
  val prune : t -> t -> t list * t

  (* splits an abstract element *)
  val split : t -> t list

  val filter : t -> (expr * cmpop * expr) -> t

  val forward_eval : t -> expr -> (float * float)

  (* 2d and 3d rendering *)
  val vertices2d : t -> var * var -> (float*float) list
  val vertices3d : t -> var * var * var -> (float*float*float) list

  (* printing *)
  val print : Format.formatter -> t -> unit

  (* volume *)
  val volume : t -> float

 end
