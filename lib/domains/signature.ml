(** Module for Abstract Domains for Constraint Programming (ADCP) they must
    feature consistency, split and precision operators. *)

(** can be raised by split operations *)
exception TooSmall

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

  val vars : t -> (Csp.typ * string) list
  (** returns the variables annoted by their type *)

  (** {1 Measure} *)

  val volume : t -> float
  (** computes the volume of an abstract element *)

  (** {1 Set-theoretic operations} *)

  val join : t -> t -> t * bool
  (** Joins two abstract elements. The boolean flag indicates if the join was
      exact. It is always sound to return false *)

  val meet : t -> t -> t option
  (** meet two abstract elements *)

  val diff : (t -> t -> t list) option
  (** substracts the second abstract element from the first (difference
      operator) if an exact operator can not be defined (None), the solver
      doesn't use the pruning features. precondition: the two abstract elements
      must be defined onto the same set of variables. *)

  val split : float -> t -> t list
  (** splits an abstract element *)

  (** {2 constraint conversion} *)

  (** domain's internal representation of a comparison *)
  type internal_constr

  val internalize :
       ?elem:t
    -> Constraint.expr * Constraint.cmpop * Constraint.expr
    -> internal_constr
  (** may use a current abstract element to simplify the constaint *)

  val externalize :
    internal_constr -> Constraint.expr * Constraint.cmpop * Constraint.expr

  (** {1 Constraint management} *)

  val filter : t -> internal_constr -> t Consistency.t
  (** filters an abstract element with respect to an arithmetic constraint, may
      raise bot found. *)

  val is_representable : internal_constr -> Kleene.t
  (** checks if a constraint is suited for this abstract domain *)

  val forward_eval : t -> Constraint.expr -> Itv.ItvQ.t
  (** computes the range of value of a given expression within an abstract
      element *)

  val to_bexpr : t -> Constraint.t
  (** transforms an abstract element into constraints *)

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
  (** constraint conversion *)

  val externalize : internal_constr -> Constraint.t

  val filter : t -> internal_constr -> t Consistency.t
  (** redefinition of filter and is_representable using boolean expression *)

  val is_representable : internal_constr -> Kleene.t
end
