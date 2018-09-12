(**********************************************************************************)
(*            Module for Abstract Domains for Constraint Programming.             *)
(*   These are abstract domains with consistency, split and precision operators.  *)
(**********************************************************************************)
type answer = Yes | Maybe | No

let and_ans a1 a2 =
  match a1, a2 with
  | No, _ | _, No -> No
  | Maybe, _ | _, Maybe -> Maybe
  | _, _ -> Yes

let not_ans = function
  | Yes -> No
  | No -> Yes
  | Maybe -> Maybe

module type AbstractCP = sig

  (*** TYPES ***)
  (* abstract elements *)
  type t

  (* returns an empty element *)
  val empty : t

  (* returns the variables *)
  val vars : t -> (Csp.annot * Csp.var) list

  (* adds an unconstrained variable to the environnement *)
  val add_var : t -> Csp.annot * Csp.var -> t

  (* returns the bounds of a variable *)
  val var_bounds : t -> Csp.var -> (Mpqf.t * Mpqf.t)

  (* returns the bound variables *)
  val bound_vars : t -> Csp.csts

  (* removes an unconstrained variable from the environnement *)
  val rem_var : t -> Csp.var -> t

  (*** PREDICATES ***)

  (* tests if an abstract element is too small to be cut *)
  val is_small : t -> bool

  val is_empty : t -> bool

  (*** OPERATIONS ***)
  val join: t -> t -> t

  (* pruning *)
  val prune : t -> t -> t list * t

  (* splits an abstract element *)
  val split : t -> t list

  val filter : t -> (Csp.expr * Csp.cmpop * Csp.expr) -> t

  val forward_eval : t -> Csp.expr -> (Mpqf.t * Mpqf.t)

  (* transforms an abstract element in constraints *)
  val to_bexpr : t -> (Csp.expr * Csp.cmpop * Csp.expr) list

  (* check if a constraint is suited for this abstract domain *)
  val is_representable : Csp.bexpr -> answer

  (* printing *)
  val print : Format.formatter -> t -> unit

  (* volume *)
  val volume : t -> float

  (* concretization function. we call it a spawner.
     useful to do tests, and to reuse the results.
     values are generated randomly *)
  val spawn : t -> Csp.instance

  (* check if an abstract element is an abstractin of an instance *)
  val is_abstraction : t -> Csp.instance -> bool


 end
