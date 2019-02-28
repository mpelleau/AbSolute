open Dbm

(** An octagon is a n-dimensional geometrical shape represented by the intersection of n n-dimensional boxes.
    It equips the DBM representation with functions to view elements in the DBM as intervals. *)

type plane = int * int

(** An element in the DBM is uniquely identified by its plane and dimension. *)
type dbm_key = int * plane

(** If the variable `v` is rotated in the plane `(d1,d2)` then returns `then_b`, otherwise `else_b`. *)
val if_rotated_else : dbm_key -> 'a -> 'a -> 'a

(** Coordinate of the canonical plane of the octagon.
    The canonical plane is the plane that is not rotated. *)
val cplane : plane

(** This function verifies that planes have a single representation.
    For example, (0,1) and (1,0) represent the same plane on the dimensions 0 and 1. *)
val well_formed_plane : plane -> bool

(** Fail with assertation if the plane is not well-formed. *)
val check_well_formed_plane : plane -> unit

(** Position of the lower bound in the DBM of an element at a specified dbm_key. *)
val lb_pos : dbm_key -> coord2D

(** Same as `lb_pos` but for the upper bound. *)
val ub_pos : dbm_key -> coord2D

(** This module signature provides function to view DBM's values as interval bounds.
    Rational: Conversion of an element in the DBM to an interval depends on the bound type. *)
module type IntervalViewDBM = sig
  type bound

  val dbm_to_lb : dbm_key -> bound -> bound
  val dbm_to_ub : dbm_key -> bound -> bound
  val lb_to_dbm : dbm_key -> bound -> bound
  val ub_to_dbm : dbm_key -> bound -> bound
end

module FloatIntervalDBM : IntervalViewDBM with type bound = Bound_float.t
module RationalIntervalDBM : IntervalViewDBM with type bound = Bound_rat.t
module IntegerIntervalDBM : IntervalViewDBM with type bound = Bound_int.t
