(** Module for Abstract Domains for Constraint Programming (ADCP) they
   must feature consistency, split and precision operators.  *)

(** can be raised by split operations *)
exception TooSmall

module type AbstractCP = sig

  (** the type of abstract elements *)
  type t

  (** returns an empty element *)
  val empty : t

  (** {1. Variables management }*)

  (** adds an unconstrained variable to the environnement *)
  val add_var : t -> Csp.decl -> t

  (** removes an unconstrained variable from the environnement *)
  val rem_var : t -> Csp.var -> t

  (** returns the variables annoted by their type *)
  val vars : t -> (Csp.annot * Csp.var) list

  (** returns the bounds of a variable *)
  val var_bounds : t -> Csp.var -> (Q.t * Q.t)

  (** returns the bound variables *)
  val bounds : t -> (Csp.var * (Q.t * Q.t)) list

  (** {1 Measure} *)

  (** tests if an abstract element is small enough with respect to `Constant.precision` *)
  val is_small : t -> bool

  (** tests if an abstract element is empty *)
  val is_empty : t -> bool

  (** {1 OPERATIONS} *)

  (** joins two abstract elements *)
  val join: t -> t -> t

  (** meet two abstract elements, may raise bot_found *)
  val meet: t -> t -> t

  (** substracts the second abstract element from the first
     (difference operator) if an exact operator can not be defined
     (None), the solver doesn't use the pruning features.
     precondition: the two abstract elements must be defined onto the
     same set of variables. *)
  val prune : (t -> t -> t list) option

  (** splits an abstract element *)
  val split : t -> Csp.ctrs -> t list

  (** filters an abstract element with respect to an arithmetic constraint,
      may raise bot found. *)
  val filter : t -> (Csp.expr * Csp.cmpop * Csp.expr) -> t Consistency.t

  (** returns the range of value of a given expression for an abstract element *)
  val forward_eval : t -> Csp.expr -> (Q.t * Q.t)

  (** transforms an abstract element into constraints *)
  val to_bexpr : t -> (Csp.expr * Csp.cmpop * Csp.expr) list

  (** checks if a constraint is suited for this abstract domain *)
  val is_representable : Csp.bexpr -> Kleene.t

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

  (** transforms an abstract element into a Picasso.Drawable.t for drawing *)
  val render : t -> Picasso.Drawable.t
end

(** An actual solvable unit *)
module type Solvable = sig
  type t (* internal representation of a solving state *)
  val propagate: t -> t Consistency.t
  val split : t -> t list
  val spawn : t -> Csp.instance
  val init : Csp.prog -> t
  type space (* internal representation of a search space *)
  val to_result : inner:bool -> space Result.t -> t -> space Result.t
end
