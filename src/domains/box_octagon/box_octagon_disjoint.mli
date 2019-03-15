open Csp
open Octagon
open Octagonal_rewriting
open Box_dom
open Abstract_domain

module type Box_octagon_disjoint_sig =
sig
  module B: Bound_sig.BOUND
  type t
  type bound = B.t

  (** `init v1 v2 c oc` initializes the reduced product of box and octagon where:
        * The variable's sets `v1` and `v2` are disjoint; the reified constraints allow to link the box and octagon domains.
        * The variables in `v1` are registered in box.
        * The variables in `v2` are registered in octagon.
        * The constraints in `c` are filtered in box or octagon according to the variables of this constraint.
        * The constraints in `oc` are octagonal constraints reified with a boolean variable registered in box.
  *)
  val init: var list -> var list -> bconstraint list -> Box_reified.box_reified_constraint list -> t

  (** This closure filters the box and octagon with regards to the (reified) constraints in `box_oct`.
      Besides reducing the domain of the variables, the entailed constraints are removed from `box_oct`. *)
  val closure: t -> t

  (** Add the constraint in the box or octagon without performing the closure.
      It throws `Bot_found` if the constraint is disentailed. *)
  val weak_incremental_closure: t -> bconstraint -> t

  val split: t -> t list
  val volume: t -> float
  val state_decomposition: t -> kleene
  val project_one: t -> var -> (bound * bound)
  val project: t -> var list -> (var * (bound * bound)) list
end

module Make
  (Box: Box_sig)
  (Octagon: Octagon_sig with module B=Box.I.B) : Box_octagon_disjoint_sig
