open Dbm

module type Closure_sig =
sig
  module DBM : DBM_sig

  val closure: DBM.t -> DBM.t

  (** Perform the incremental closure of the DBM from a constraint.
      The complexity is O(n^2) where `n` is the dimension of the DBM.
      It does not check for entailment before running (this is done in `Octagon`). *)
  val incremental_closure: DBM.t -> DBM.bound dbm_constraint -> DBM.t

  val is_consistent : DBM.t -> DBM.t
end

module ClosureZ : Closure_sig
module ClosureQ : Closure_sig
module ClosureF : Closure_sig
