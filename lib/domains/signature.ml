(** Module signature for Abstract Domains for Constraint Programming (ADCP) they
    must feature consistency, split and precision operators. *)

(** can be raised by split operations *)
exception Too_small

module type Numeric = sig
  (** the type of (non-bottom) abstract elements *)
  type t

  val empty : t
  (** returns an empty element *)

  (** {1 Variables management}*)

  val add_var : t -> Csp.decl -> t
  (** adds an unconstrained variable to the environnement *)

  val rm_var : t -> string -> t
  (** removes a variable from the environnement *)

  val vars : t -> (Csp.typ * string * Dom.t) list
  (** returns the variables annoted by their type and their definition domain *)

  (** {1 Measure} *)

  val volume : t -> float
  (** computes the volume of an abstract element *)

  (** {1 Set-theoretic operations} *)

  val join : t -> t -> t * bool
  (** Joins two abstract elements. The boolean flag indicates if the join was
      exact. It is always sound to return false *)

  val join_list : t list -> t * bool
  (** Joins a list of [n] abstract elements. May be faster than joining
      pairwise. The boolean flag indicates if the join was exact. It is always
      sound to return false *)

  val meet : t -> t -> t option
  (** meet two abstract elements *)

  val diff : (t -> t -> t list) option
  (** substracts the second abstract element from the first (difference
      operator) if an exact operator can not be defined (None), the solver
      doesn't use the pruning features. precondition: the two abstract elements
      must be defined onto the same set of variables. *)

  val split_along : ?prec:float -> string -> t -> t list
  (** splits an abstract element along the specified variable *)

  val split : ?prec:float -> t -> t list
  (** splits an abstract element according to an internal heuristic *)

  val split_diff : ?prec:float -> t -> t list * Tools.VarSet.t
  (** splits an abstract element according to an internal heuristic, also
      returns a set of variable that have been affected by the split *)

  (** {2 constraint conversion} *)

  (** domain's internal representation of a comparison *)
  type internal_constr

  val internalize : ?elem:t -> Constraint.comparison -> internal_constr
  (** converts a comparison to the domain's internal represenatation. May use a
      current abstract element to simplify the constaint *)

  val externalize : internal_constr -> Constraint.comparison

  (** {1 Constraint management} *)

  val sat : Instance.t -> internal_constr -> bool
  (** satisfaction test on the internal constraint representation *)

  val filter : t -> internal_constr -> t Consistency.t
  (** filters an abstract element with respect to an arithmetic constraint *)

  val filter_diff : t -> internal_constr -> (t * Tools.VarSet.t) Consistency.t
  (** same as filter but also return a set of variables that had their domains
      effectively reduced *)

  val is_representable : internal_constr -> Kleene.t
  (** checks if a constraint is suited for this abstract domain *)

  val eval : t -> Expr.t -> Q.t * Q.t
  (** computes the range of value of a given expression within an abstract
      element. may raise Top_found *)

  val to_constraint : t -> Constraint.t
  (** transforms an abstract element into constraints. may raise Top_found *)

  val spawn : t -> Csp.instance
  (** Random concretization function. useful to do tests, and to reuse the
      results. values are generated uniformly when possible *)

  val is_abstraction : t -> Csp.instance -> bool
  (** check if an abstract element is an abstraction of an instance *)

  val print : Format.formatter -> t -> unit
  (** printing *)

  val render : t -> Picasso.Drawable.t
  (** transforms an abstract element into a Picasso.Drawable.t for drawing *)
end

(** Abstract domain with full handling of boolean expressions instead of only
    numeric comparisons *)
module type Domain = sig
  include Numeric

  (** domain's internal representation of a constraint *)
  type internal_constr

  val internalize : ?elem:t -> Constraint.t -> internal_constr
  (** Converts a constraint to the domain's internal representation. May use a
      current abstract element to simplify the constaint *)

  val externalize : internal_constr -> Constraint.t

  val sat : Instance.t -> internal_constr -> bool
  (** satisfaction test on the internal constraint representation *)

  val filter : t -> internal_constr -> (t * internal_constr) Consistency.t
  (** redefinition of filter using boolean expression, it also computes a
      potentially simplified equivalent constraint e.g:
      - false || c <=> c
      - true && c <=> c *)

  val filter_diff :
    t -> internal_constr -> (t * internal_constr * Tools.VarSet.t) Consistency.t
  (** same as filter but also returns the set of variables that have been
      effectively filtered *)

  val is_representable : internal_constr -> Kleene.t
  (** checks if a constraint can be encoded without loss of precision;
      i.e {m \forall x, \gamma(\rho(x,c)) = \{v \in \gamma(x) |
      c(v)\}}*)
end

module type Propagator = sig
  module Make : functor (D : Domain) -> sig
    type space = D.t

    type t

    val init : ?verbose:bool -> Csp.t -> t

    val propagate : t -> t Consistency.t

    val split : ?prec:float -> t -> t list

    val spawn : t -> Csp.instance

    val to_result : inner:bool -> space Result.t -> t -> space Result.t

    val to_csp : t -> Csp.t
  end
end
