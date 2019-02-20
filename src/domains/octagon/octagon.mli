open Dbm

(** An octagon is a n-dimensional geometrical shape represented by the intersection of n n-dimensional boxes.
    It equips the DBM representation with functions to view elements in the DBM as intervals. *)

type plane = int * int

(** An element in the DBM is uniquely identified by its plane and dimension. *)
type key = int * plane

(** If the variable `v` is rotated in the plane `(d1,d2)` then returns `then_b`, otherwise `else_b`. *)
val if_rotated_else : key -> 'a -> 'a -> 'a

(** Coordinate of the canonical plane of the octagon.
    The canonical plane is the plane that is not rotated. *)
val cplane : plane

(** This function verifies that planes have a single representation.
    For example, (0,1) and (1,0) represent the same plane on the dimensions 0 and 1. *)
val well_formed_plane : plane -> bool

(** Fail with assertation if the plane is not well-formed. *)
val check_well_formed_plane : plane -> unit

(** Position of the lower bound in the DBM of an element at a specified key. *)
val lb_pos : key -> coord2D

(** Same as `lb_pos` but for the upper bound. *)
val ub_pos : key -> coord2D

(** This module signature provides function to view DBM's values as interval bounds.
    Rational: Conversion of an element in the DBM to an interval depends on the bound type. *)
module type IntervalViewDBM = sig
  module B : Bound_sig.BOUND
  type bound = B.t
  type itv

  val dbm_to_lb : key -> bound -> bound
  val dbm_to_ub : key -> bound -> bound
  val lb_to_dbm : key -> bound -> bound
  val ub_to_dbm : key -> bound -> bound

  val itv_to_range : itv -> (bound * bound)
  val range_to_itv : bound -> bound -> itv
end

module FloatIntervalDBM : IntervalViewDBM
module RationalIntervalDBM : IntervalViewDBM
module IntegerIntervalDBM : IntervalViewDBM

module Make
  (IntervalView: IntervalViewDBM)
  (Closure: Closure.Closure_sig with module DBM = Dbm.Make(IntervalView.B))
  (Rewriter: Octagonal_rewriting.Rewriter_sig) :
sig
  type t
  type bound

  (** `init vars constraints relaxed`
      Initialize the octagon with the set of variables `vars`.
      The octagonal `constraints` defined on `vars` are added into the octagon.
      The octagonal constraints are annotated by `true`: they are subsumed in the octagon. *)
  val init: Csp.var list -> Csp.bconstraint list -> ((bool * Csp.bconstraint) list * t)

  val empty: t

  (** Extend the octagon with a new variable. *)
  val extend_one: t -> Csp.var -> t

  val update: t -> Octagonal_rewriting.octagonal_constraint -> unit
  val join_constraint: t -> Csp.bconstraint -> bool

  (** Set the lower bound of the variable `k` in the DBM.
      The value `v` is the lower bound of the variable at key `k`, and is processed to fit in the DBM. *)
  val set_lb: t -> key -> bound -> unit

  (** Same as `set_lb` but for the upper bound. *)
  val set_ub: t -> key -> bound -> unit

  (** Lower bound of the variable at key `k`.
      The value is computed directly from the DBM with care on rounding. *)
  val lb: t -> key -> bound

  (** Same as `lb` but for the upper bound. *)
  val ub: t -> key -> bound

  (** Perform the closure of the DBM. *)
  val closure: t -> unit
end
