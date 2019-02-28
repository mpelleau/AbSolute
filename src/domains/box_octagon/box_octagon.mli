open Csp
open Octagonalisation
open Octagon
open Octagonal_rewriting
open Box_dom

type reified_octagonal = (var * octagonal_constraint list)

module type Box_octagon_sig =
sig
  type t
  type bound

  (** `init v1 v2 c oc` initializes the reduced product of box and octagon where:
        * `v2` is a subset of `v1`.
        * The variables in `v1` are registered in box.
        * The variables in `v2` are registered in octagon.
        * The constraints in `c` are primarily filtered in octagon, with an automatic fall back on the box.
        * The constraints in `oc` are octagonal constraints reified with a boolean.
      NOTE: `oc` is very specific for RCPSP, it should not be in this signature, but we want to avoid to either (i) check every time if the reified constraint is octagonal, or (ii) annotate the constraints with their domains.
  *)
  val init: var list -> var list -> bconstraint list -> reified_octagonal list -> t
end

module Make
  (B: Bound_sig.BOUND)
  (RotationStrategy: Octagonalisation)
  (Octagon: Octagon_sig with type bound=B.t)
  (Box: Box_sig with type bound=B.t and type I.bound=B.t) : Box_octagon_sig
