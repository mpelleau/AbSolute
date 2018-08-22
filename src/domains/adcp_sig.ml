(**********************************************************************************)
(*            Module for Abstract Domains for Constraint Programming.             *)
(*   These are abstract domains with consistency, split and precision operators.  *)
(**********************************************************************************)

open Csp

type answer = Yes | Maybe | No
                          
module type AbstractCP = sig

  (*** TYPES ***)
  (* abstract elements *)
  type t

  (* (\* expression and constraint conversion *\) *)
  (* type expr *)
  (* type cmp *)

  (* val translate_expr : Csp.expr -> expr *)

  (* val translate_cons : Csp.expr * Csp.cmpop * Csp.expr -> expr * cmpop * expr *)

  (*** INSTANCIATION ***)

  (* returns an empty element *)
  val empty : t

  (* returns the variables *)
  val vars : t -> var list

  (* adds an unconstrained variable to the environnement *)
  val add_var : t -> typ * var -> t

  (* returns the bounds of a variable *)
  val var_bounds : t -> var -> (Mpqf.t * Mpqf.t)

  (* returns the bound variables *)
  val bound_vars : t -> csts

  (* removes an unconstrained variable to the environnement *)
  val rem_var : t -> var -> t

  (*** PREDICATES ***)

  (* tests if an abstract element is too small to be cut *)
  val is_small : t -> bool

  val is_empty : t -> bool

  val is_enumerated : t -> bool

  (*** OPERATIONS ***)
  val join: t -> t -> t

  (* pruning *)
  val prune : t -> t -> t list * t

  (* splits an abstract element *)
  val split : t -> t list

  val filter : t -> (expr * cmpop * expr) -> t

  val forward_eval : t -> expr -> (Mpqf.t * Mpqf.t)

  (* transforms an abstract element in constraints *)
  val to_bexpr : t -> (expr * cmpop * expr) list

  (* check if a constraint is suited for this abstract domain *)
  val is_representable : bexpr -> answer

  (* printing *)
  val print : Format.formatter -> t -> unit

  (* volume *)
  val volume : t -> float

 end
