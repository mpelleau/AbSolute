(** This module provides functions to detect and rewrite arbitrary constraint into an equivalent and relaxed octagonal constraints, if possible. *)
open Csp
open Dbm

module type Rewriter_sig = functor (B: Bound_sig.BOUND) ->
sig
  module B: Bound_sig.BOUND
  type t

  (** Initialize the rewriter with the map between variable's name and DBM interval. *)
  val init: (var * dbm_interval) list -> t

  val var_dbm_to_box: t -> dbm_interval -> var
  val var_box_to_dbm: t -> var -> dbm_interval

  (** Create octagonal constraints from an initial constraint.
    If the list is empty, it is not possible to rewrite the constraint.
    Multiple elements mean the constraint has been decomposed into several octagonal constraints. *)
  val rewrite: t -> bconstraint -> (B.t dbm_constraint) list

  (** Relax the constraint into an octagonal version, if possible.
      The list returned is empty if the constraint cannot be relaxed or is already octagonal.
      For continuous bound, it rewrites strict inequalities `<`,`>` into the inequality `<=`,`>=`.
      For discrete bound, it always returns the empty list. *)
  val relax: t -> bconstraint -> (B.t dbm_constraint) list
end with module B=B

module Rewriter: Rewriter_sig
