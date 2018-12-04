(**********************************************************************************)
(*            Module for Abstract Domains for Constraint Programming (ADCP).      *)
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
  (** abstract elements *)
  type t

  (** returns an empty element *)
  val empty : t

  (** returns the variables annoted by their type *)
  val vars : t -> (Csp.annot * Csp.var) list

  (** adds an unconstrained variable to the environnement *)
  val add_var : t -> Csp.annot * Csp.var -> t

  (** returns the bounds of a variable *)
  val var_bounds : t -> Csp.var -> (Mpqf.t * Mpqf.t)

  (** returns the bound variables *)
  val bound_vars : t -> Csp.csts

  (** removes an unconstrained variable from the environnement *)
  val rem_var : t -> Csp.var -> t

  (*** PREDICATES ***)

  (** tests if an abstract element is small enough with respect to `Constant.precision` *)
  val is_small : t -> bool

  (** tests if an abstract element is empty *)
  val is_empty : t -> bool

  (*** OPERATIONS ***)
  (** joins two abstract elements *)
  val join: t -> t -> t

  (** meet two abstract elements, may raise bot_found *)
  val meet: t -> t -> t

  (** substracts the second abstract element from the first (difference operator)
      if an exact operator can not be defined (None), the solver doesn't use the pruning
      features.
      precondition: the two abstract elements must be defined onto the same set of variables. *)
  val prune : (t -> t -> t list) option

  (** splits an abstract element *)
  val split : t -> Csp.ctrs -> t list

  (** Pizza splits an abstract element around the given point *)
  val split_on : t -> Csp.ctrs -> Csp.instance -> t list

  (** filters an abstract element with respect to an arithmetic constraint,
      may raise bot found. *)
  val filter : t -> (Csp.expr * Csp.cmpop * Csp.expr) -> t

  (** returns the range of value of a given expression for an abstract element *)
  val forward_eval : t -> Csp.expr -> (Mpqf.t * Mpqf.t)

  (** transforms an abstract element into constraints *)
  val to_bexpr : t -> (Csp.expr * Csp.cmpop * Csp.expr) list

  (** checks if a constraint is suited for this abstract domain *)
  val is_representable : Csp.bexpr -> answer

  (** printing *)
  val print : Format.formatter -> t -> unit

  (** computes the volume of an abstract element *)
  val volume : t -> float

  (** concretization function. we call it a spawner.
      useful to do tests, and to reuse the results.
      values are generated randomly *)
  val spawn : t -> Csp.instance

  (** check if an abstract element is an abstraction of an instance *)
  val is_abstraction : t -> Csp.instance -> bool

  (** Skrinks the abstract element in every direction by the given value. *)
  val shrink : t -> Mpqf.t -> t

 end
