open Abstract_domain
open Dbm

module type Octagon_sig =
sig
  module DBM : DBM_sig
  module B = DBM.B
  type bound = B.t
  type t

  (** Create an octagon of `n` number of variables. *)
  val init: int -> t

  val copy: t -> t

  (** Given an octagonal constraint, return `True` if it is entailed by the octagon, `False` if it is disentailed, and `Unknown` if it be entailed or disentailed in the future. *)
  val entailment: t -> bound dbm_constraint -> kleene

  (** Perform the closure of the DBM. *)
  val closure: t -> t

  (** Perform the incremental closure of the DBM with the constraint. *)
  val incremental_closure: t -> bound dbm_constraint -> t

  (** Add the octagonal constraint in the octagon, if it is not entailed and without closing the DBM.
      It throws `Bot_found` if the constraint is disentailed (see `entailment`). *)
  val weak_incremental_closure: t -> bound dbm_constraint -> t

  (** Low-level access to the DBM as a list. *)
  val dbm_as_list: t -> bound list

  val split: t -> t list

  val state_decomposition: t -> kleene

  (** See `DBM.project` *)
  val project: t -> dbm_interval -> (bound * bound)

  val volume: t -> float
end

module OctagonZ(SPLIT: Octagon_split.Octagon_split_sig) : Octagon_sig
module OctagonQ(SPLIT: Octagon_split.Octagon_split_sig) : Octagon_sig
module OctagonF(SPLIT: Octagon_split.Octagon_split_sig) : Octagon_sig

module Make
  (Closure: Closure.Closure_sig)
  (SPLIT: Octagon_split.Octagon_split_sig) : Octagon_sig
