open Csp
open Abstract_domain
open Box_dom

(* This module equips `Box_dom` with reified constraints. *)

type box_reified_constraint = var * bconstraint list

module type Box_reified_sig =
sig
  type t
  type bound
  module I: Itv_sig.ITV with type bound = bound
  type itv = I.t

  val init: var list -> bconstraint list -> box_reified_constraint list -> t
  val get: t -> Csp.var -> itv
  val project: (Csp.var -> bool) -> t -> t
  val weak_incremental_closure: t -> Csp.bconstraint -> t
  val closure: t -> t
  val incremental_closure: t -> Csp.bconstraint -> t
  val entailment: t -> Csp.bconstraint -> kleene
  val volume: t -> float
  val state_decomposition: t -> kleene
  val print: Format.formatter -> t -> unit
end

module Make
  (B: Bound_sig.BOUND)
  (Box: Box_sig with type bound=B.t) : Box_reified_sig

module BoxReifiedZ : Box_reified_sig with type bound = Bound_int.t
module BoxReifiedQ : Box_reified_sig with type bound = Bound_rat.t
module BoxReifiedF : Box_reified_sig with type bound = Bound_float.t
