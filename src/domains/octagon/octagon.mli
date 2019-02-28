open Interval_view_dbm
open Octagonal_rewriting
open Csp
open Abstract_domain

module type Octagon_sig =
sig
  type t
  type bound

  (** `init vars constraints`
      Create an octagon with the set of variables `vars`.
      The dimension of the returned octagon is `length vars`.
      The octagonal `constraints` defined on `vars` are added into the octagon.
      The octagonal constraints are annotated by `true`: they are subsumed in the octagon.*)
  val init: var list -> bconstraint list -> ((bool * bconstraint) list * t)

  val empty: t

  (** Extend the octagon with a new variable. *)
  val extend_one: t -> var -> t

  val update: t -> octagonal_constraint -> unit
  val meet_constraint: t -> bconstraint -> bool

  (* Fold over the canonical variables of the octagon.
     Note that rotated variable's names do not belong to this module. *)
  val fold_vars: (var -> dbm_key -> 'a -> 'a) -> 'a -> t -> 'a

  (* Iterate over the canonical variable of the octagon. *)
  val iter_vars: (var -> dbm_key -> unit) -> t -> unit

  (** Set the lower bound of the variable `k` in the DBM.
      The value `v` is the lower bound of the variable at dbm_key `k`, and is processed to fit in the DBM. *)
  val set_lb: t -> dbm_key -> bound -> unit

  (** Same as `set_lb` but for the upper bound. *)
  val set_ub: t -> dbm_key -> bound -> unit

  (** Lower bound of the variable at dbm_key `k`.
      The value is computed directly from the DBM with care on rounding. *)
  val lb: t -> dbm_key -> bound

  (** Same as `lb` but for the upper bound. *)
  val ub: t -> dbm_key -> bound

  (** Perform the closure of the DBM. *)
  val closure: t -> unit

  (** Low-level access to the DBM as a list. *)
  val dbm_as_list: t -> bound list

  (** Given an octagonal constraint, return `True` if it is entailed by the octagon, `False` if it is disentailed, and `Unknown` if it be entailed or disentailed in the future. *)
  val entailment: t -> octagonal_constraint -> kleene
end

module OctagonZ : Octagon_sig with type bound = Bound_int.t
module OctagonQ : Octagon_sig with type bound = Bound_rat.t
module OctagonF : Octagon_sig with type bound = Bound_float.t

module Make
  (B: Bound_sig.BOUND)
  (IntervalView: IntervalViewDBM with type bound=B.t)
  (Closure: Closure.Closure_sig with module DBM = Dbm.Make(B))
  (Rewriter: Octagonal_rewriting.Rewriter_sig) : Octagon_sig
