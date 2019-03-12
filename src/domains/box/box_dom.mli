open Csp
open Var_store
open Abstract_domain

module type Box_sig =
sig
  type t
  type bound
  module I: Itv_sig.ITV with type bound = bound
  type itv = I.t

  (** Initialize the box with variables and constraints.
      The box returned is not closed, therefore you should apply `closure` to this box right after initialization. *)
  val init: var list -> bconstraint list -> t

  (* Get the interval of the variable `v`. *)
  val get: t -> var -> itv

  (* Projection of the variables `v` in the `box` satisfying `f v`. *)
  val project: (var -> bool) -> t -> t

  (** Add the constraint `c` into the box.
      Precondition: The variables in `c` must range over the current store.
      The store of variables is left unchanged, and `closure` should be applied. *)
  val weak_incremental_closure: t -> bconstraint -> t

  (** Closure of the store with regards to all constraints in the box.
      A fixed point is reached when no constraint can be propagated anymore.
      The entailed constraints are removed from the box.
      Throw `Bot_found` one of the constraints is unsatisfiable. *)
  val closure: t -> t

  (** Equivalent to `closure (weak_incremental_closure box c)`. *)
  val incremental_closure: t -> bconstraint -> t

  (** Return the entailment status of the constraint in `box`. *)
  val entailment: t -> bconstraint -> kleene

  (** Compute the volume of the box.
      We return `1.` if the box only contain singleton domain. *)
  val volume: t -> float

  (** Characterize the state of the box.
      `True` is returned when all the constraints are entailed.
      `False` is never returned because `Bot_found` is raised in case of unsatisfiability (in `incremental_closure` or `closure`). *)
  val state_decomposition: t -> kleene

  (** Print the variables store and the remaining constraints (those not yet entailed). *)
  val print: Format.formatter -> t -> unit
end

module Make
  (B: Bound_sig.BOUND)
  (I: Itv_sig.ITV with type bound=B.t)
  (Store: Var_store_sig with type cell=I.t)
  (Closure: Hc4.Box_closure_sig with module Store=Store) : Box_sig

module BoxZ : Box_sig with type bound = Bound_int.t
module BoxQ : Box_sig with type bound = Bound_rat.t
module BoxF : Box_sig with type bound = Bound_float.t
