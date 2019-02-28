(** This module provides functions to detect and rewrite arbitrary constraint into an equivalent and relaxed octagonal constraints, if possible. *)
open Csp

type sign = Positive | Negative

(** Constraint of the form `±x - ±y <= c`. *)
type octagonal_constraint = {
  x: sign * Csp.var;
  y: sign * Csp.var;
  c: Bound_rat.t;
}

val vars_of: octagonal_constraint -> Csp.var list

(** Given an octagonal constraint, we reverse the signs to obtain the opposite octagonal constraint.
    Note that the bound `c` is left unchanged, and thus the reversed constraint describes the opposite plane. *)
val reverse_sign: octagonal_constraint -> octagonal_constraint

(** Version of `create` only for constraints with two variables of the form `±x - ±y <= c`. *)
val create_if_two_vars: bconstraint -> octagonal_constraint option

(** Create an octagonal constraint if the syntax of the constraint is of the form `±x - ±y <= c` or `±x <= c`.
    This function also serves to check if a constraint is in octagonal form.
    See also the functions `normalize` and `rewrite` to turn a constraint into its octagonal form. *)
val try_create: bconstraint -> octagonal_constraint option

(** Simplify negations inside the expression:
      1. Remove double negation: --x ~> x | x - -y ~> x + y.
      2. Propagate negation on constants.
      3. Push negation inside expressions: -(x + y) ~> -x - y.
    The goal is to obtain an expression such that it is in a normal form. *)
val normalize_expr: expr -> expr
val normalize: bconstraint -> bconstraint

(** Rewrite a constraint into an octagonal form, or into a form closer to an octagonal constraint, if possible.
    It rewrites a constraint defined with `>=` and `>` into a constraint using `<=` and `<`.
    Equality `=` is rewritten into constraints with `<=` and `>=`.
    This function is generic with regards to the bound type (Q,F,Z). *)
val generic_rewrite: bconstraint -> bconstraint list

val unwrap_all: (octagonal_constraint option) list -> octagonal_constraint list

module type Rewriter_sig =
sig
  (** Create octagonal constraints from an initial constraint.
      If the list is empty, it is not possible to rewrite the constraint.
      Multiple elements mean the constraint has been decomposed into octagonal constraint. *)
  val rewrite: bconstraint -> octagonal_constraint list

  (** Relax the constraint into an octagonal version, if possible.
      The list returned is empty if the constraint cannot be relaxed. *)
  val relax: bconstraint -> octagonal_constraint list
end

module RewriterZ : Rewriter_sig
module RewriterQF : Rewriter_sig
