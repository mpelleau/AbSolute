open Csp
open Var_store
open Abstract_domain

module type Box_sig =
sig
  type t
  type bound
  module I: Itv_sig.ITV with type bound = bound
  type itv = I.t

  val init: var list -> t
  val volume: t -> float

  (* Get the interval of the variable `v`. *)
  val get: t -> var -> itv

  (* Projection of the variables `v` in the `box` satisfying `f v`. *)
  val project: (var -> bool) -> t -> t

  (** Intersect the current box with `(var, value)`.
      It fails if the variable is not registered in the box. *)
  val meet_var: var -> itv -> t -> t

  (** Filter a constraint in `box`.
      Throw `Bot_found` if the constraint is unsatisfiable. *)
  val closure: t -> bconstraint -> t

  (** Return the entailment status of the constraint in `box`. *)
  val entailment: t -> bconstraint -> kleene
end

module Make
  (B: Bound_sig.BOUND)
  (I: Itv_sig.ITV with type bound=B.t)
  (Store: Var_store_sig with type cell=I.t)
  (Closure: Hc4.Box_closure_sig with module Store=Store) : Box_sig

module BoxZ : Box_sig with type bound = Bound_int.t
module BoxQ : Box_sig with type bound = Bound_rat.t
module BoxF : Box_sig with type bound = Bound_float.t
