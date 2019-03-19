open Csp
open Var_store
open Abstract_domain

module type Box_sig =
sig
  type t
  module I: Itv_sig.ITV
  type itv = I.t
  type bound = I.B.t

  (** Initialize the box with variables and constraints.
      The box returned is not closed, therefore you should apply `closure` to this box right after initialization. *)
  val init: var list -> bconstraint list -> t

  (* Get the interval of the variable `v`. *)
  val get: t -> var -> itv

  (* Projection of the variables according to their names. *)
  val project_one: t -> var -> (bound * bound)
  val project: t -> var list -> (var * (bound * bound)) list

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

  (** See `Abstract_domain`. *)
  val split: t -> t list
end

module type Box_functor = functor (B: Bound_sig.BOUND) -> Box_sig with module I.B = B

module Make
  (B: Bound_sig.BOUND)
  (INTERVAL: Itv_sig.Itv_functor)
  (STORE: Var_store_functor)
  (CLOSURE: Hc4.Box_closure_sig)
  (SPLIT: Box_split.Box_split_sig) : Box_sig

module Box_base(SPLIT: Box_split.Box_split_sig) : Box_functor
