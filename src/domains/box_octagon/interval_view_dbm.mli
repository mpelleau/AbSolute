(*open Dbm

(** An octagon is a n-dimensional geometrical shape represented by the intersection of n n-dimensional boxes.
    It equips the DBM representation with functions to view elements in the DBM as intervals. *)

(** `true` is the variable at `coord2D` is rotated. *)
val is_rotated : coord2D -> bool

(** This module signature provides function to view DBM's values as interval bounds.
    Rational: Conversion of an element in the DBM to an interval depends on the bound type. *)
module type Interval_view_sig = functor (B: Bound_sig.BOUND) ->
sig
  module B: Bound_sig.BOUND
  type bound = B.t

  val dbm_to_lb : coord2D -> bound -> bound
  val dbm_to_ub : coord2D -> bound -> bound
  val lb_to_dbm : coord2D -> bound -> bound
  val ub_to_dbm : coord2D -> bound -> bound
end with module B=B

module Interval_view : Interval_view_sig
*)