(** This module defines the constraint language, and some basic operations over
    it*)

(** arithmetic comparison operators *)
type cmpop = EQ | LEQ | GEQ | NEQ | GT | LT

(** boolean comparison : e1 <> e2 *)
type comparison = Expr.t * cmpop * Expr.t

(** boolean expressions *)
type 'a boolean =
  | Cmp of 'a
  | And of 'a boolean * 'a boolean
  | Or of 'a boolean * 'a boolean
  | Not of 'a boolean

(** type for constraints *)
type t = comparison boolean

(** {1 Constructors} *)

(** {2 Comparisons}*)

val leq : Expr.t -> Expr.t -> t
(** e1 <= e2 *)

val lt : Expr.t -> Expr.t -> t
(** e1 < e2 *)

val geq : Expr.t -> Expr.t -> t
(** e1 >= e2 *)

val gt : Expr.t -> Expr.t -> t
(** e1 > e2 *)

val eq : Expr.t -> Expr.t -> t
(** e1 = e2 *)

val neq : Expr.t -> Expr.t -> t
(** e1 <> e2 *)

(** {2 Boolean formulae}*)

val and_ : t -> t -> t
(** b1 && b2 *)

val or_ : t -> t -> t
(** b1 || b2 *)

val imply : t -> t -> t
(** b1 => b2 *)

val not_ : t -> t
(** not b *)

(** {2 Utilities} *)

val assign : string -> Q.t -> t
(** constraint for variable assignment by a constant *)

val inside : Expr.t -> Expr.t -> Expr.t -> t
(** constraint for 'e \in \[low;high\]' *)

val outside : Expr.t -> Expr.t -> Expr.t -> t
(** constraint for 'e \notin \[low;high\]' *)

val inside_cst : string -> Q.t -> Q.t -> t
(** same as inside but with string and rationals instead of expressions' *)

val outside_cst : string -> Q.t -> Q.t -> t
(** same as outside but with string and rationals instead of expressions' *)

val of_instance : Instance.t -> t
(** conversion of a point [p] to a conjunctive constraint whose only solution is
    the point p.

    @raise [Invalid_arg] if the instance is empty *)

val convex_hull : Instance.t list -> t
(** Builds the conjuntive constraint defined by the linear system corresponding
    to the convex hull of the given set of points. The list of instance must not
    be empty and all the instances should be defined over the same set of keys,
    otherwise the behaviour in undefined. *)

(** {1 Operations} *)

val inv_cmp : cmpop -> cmpop
(** cmp operator inversion *)

val neg_cmp : cmpop -> cmpop
(** comparison operator negation *)

val cmp_to_fun : cmpop -> Q.t -> Q.t -> bool
(** returns the rational function corresponding to the cmp operator *)

val nullify_rhs : comparison -> comparison
(** [nullify c] computes a comparison equivalent to [c], with its
    right-hand-side being 0 *)

val neg : t -> t
(** constraint negation *)

val remove_not : t -> t
(** rewrites a constraint into an equivalent constraint without the logical
    operator 'Not' *)

val collect_vars : t -> int Tools.VarMap.t
(** Returns all the variables appearing in a constraint as a map where to each
    variable is associated the (integer) number of occurences *)

val support : t -> string list
(** Returns all the variables appearing in a constraint as a list *)

val replace : t -> string -> Expr.t -> t
(** [replace constr var expr] builds a new constraint identical to [constr]
    where all the occurences of the variable [var] are replaced by the
    expression [expr] *)

val fix_var : t -> string -> Q.t -> t
(** [fix_var constr var cst] builds a new constraint identical to [constr] where
    all the occurences of the variable [var] are replaced by the constant [cst] *)

val eval_comparison : comparison -> Instance.t -> bool
(** Evaluates the constraint a the given point.

    @raise [Invalid_arg]
      if a division by zero occurs or if an exponentitation by a non integer
      exposant is made. *)

val eval : t -> Instance.t -> bool
(** Evaluates the constraint a the given point.

    @raise [Invalid_arg]
      if a division by zero occurs or if an exponentitation by a non integer
      exposant is made. *)

(** {1 Printing} *)

val pp_cmpop : Format.formatter -> cmpop -> unit
(** comparison operator printer *)

val pp_comparison : Format.formatter -> comparison -> unit
(** comparison printer *)

val print : Format.formatter -> t -> unit
(** printer *)

val to_string : t -> string
(** Conversion to a string *)

(** Classic infix boolean operators are redefined on [t]. *)
module Operators : sig
  val ( > ) : Expr.t -> Expr.t -> t

  val ( < ) : Expr.t -> Expr.t -> t

  val ( >= ) : Expr.t -> Expr.t -> t

  val ( <= ) : Expr.t -> Expr.t -> t

  val ( = ) : Expr.t -> Expr.t -> t

  val ( <> ) : Expr.t -> Expr.t -> t
end
