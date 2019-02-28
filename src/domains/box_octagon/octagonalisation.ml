open Csp
open Interval_view_dbm

module type Octagonalisation =
sig
  (** Given the set of variables in the octagon and a set of constraints, the octagonalisation strategy returns the rotated constraints and the new variables introduced.
      The rotated variables represent a box rotated at 45 degrees (we view a 2D octagon as the intersection of two boxes). *)
  val rotate: var list -> bconstraint list -> (bconstraint list * (var * dbm_key) list)
end

module NoRotation =
struct
  let rotate vars constraints = [], []
end

module FullRotation =
struct
  let rotate vars constraints = [], []
end

module ConstraintBased =
struct
  let rotate vars constraints = [], []
end

module Random =
struct
  let rotate vars constraints = [], []
end

module StrongestLink =
struct
  let rotate vars constraints = [], []
end

module Promising =
struct
  let rotate vars constraints = [], []
end
