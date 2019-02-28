(** The octagonalisation consists in explictly representing the variables modelling rotated planes in the octagon.
  Consider the octagonal constraint `X - Y <= 10`, the variables `X` and `Y` can be depicted as a box in the canonical plane of the octagon.
  The idea of octagonalisation is to associate two new variables by considering the plane (X,Y) rotated at 45°.
  The different strategies consist in selecting which variables we want to represent explicitly.
  NOTE: the goal is to filter the rotated constraints in the Box abstract domain. *)

open Csp
open Interval_view_dbm

module type Octagonalisation =
sig
  (** Given the set of variables in the octagon and a set of constraints, the octagonalisation strategy returns the rotated constraints and the new variables introduced.
      The rotated variables represent a box rotated at 45 degrees (we view a 2D octagon as the intersection of two boxes). *)
  val rotate: var list -> bconstraint list -> (bconstraint list * (var * dbm_key) list)
end

(** No variable is created and no constraint is rotated. *)
module NoRotation: Octagonalisation

(** Every plane with a variable appearing in at least one constraint is rotated. *)
module FullRotation: Octagonalisation

(** The rotated plane (i,j) is only created if the variables v_i and v_j appear in the same constraint. *)
module ConstraintBased: Octagonalisation

(** A single random plane is generated. *)
module Random: Octagonalisation

(** A single plane (i,j) is generated, such that where the variables v_i and v_j are the most appearing in the same constraint the most often. *)
module StrongestLink: Octagonalisation

(** A single plane (i,j) is generated, such that it maximizes the number of "promising schema" in the constraint.
    A promising schema is an expression in the rotated constraint that can be easily simplified. *)
module Promising: Octagonalisation
