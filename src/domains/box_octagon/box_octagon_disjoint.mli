open Csp
open Octagon
open Octagonal_rewriting
open Box_dom
open Abstract_domain

type reified_octagonal = (var * octagonal_constraint list)

module type Box_octagon_disjoint_sig =
sig
  module B: Bound_sig.BOUND
  type t
  type bound = B.t

  (** `init v1 v2 c oc` initializes the reduced product of box and octagon where:
        * The problem is entirely described by the octagonal variables in `v2` (no branching occurs on box variables).
        * The variable's sets `v1` and `v2` are disjoint; the reified constraints allow to link the box and octagon domains.
        * The variables in `v1` are registered in box.
        * The variables in `v2` are registered in octagon.
        * The constraints in `c` are primarily filtered in octagon, with an automatic fall back on the box.
        * The constraints in `oc` are octagonal constraints reified with a boolean.
      NOTE: `oc` is very specific for RCPSP, it should not be in this signature, but we want to avoid to either (i) check every time if the reified constraint is octagonal, or (ii) annotate the constraints with their domains.
  *)
  val init: var list -> var list -> bconstraint list -> reified_octagonal list -> t

  val closure: t -> t
  val split: t -> t list
  val volume: t -> float
  val state_decomposition: t -> kleene
  val project_one: t -> var -> (bound * bound)
  val project: t -> var list -> (var * (bound * bound)) list
  val meet_var: t -> var -> (bound * bound) -> t
end

module Make
  (B: Bound_sig.BOUND)
  (Octagon: Octagon_sig with type bound=B.t)
  (Box: Box_sig with type bound=B.t and type I.bound=B.t) : Box_octagon_disjoint_sig
