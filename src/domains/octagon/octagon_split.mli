open Dbm

module type Variable_order = functor (DBM : DBM_sig) ->
sig
  module DBM : DBM_sig
  val select: DBM.t -> dbm_interval option
end with module DBM=DBM

module type Value_order = functor (DBM: DBM_sig) ->
sig
  module DBM : DBM_sig
  val select: DBM.t -> dbm_interval -> DBM.bound
end with module DBM=DBM

module type Distributor = functor (DBM: DBM_sig) ->
sig
  module DBM : DBM_sig
  val distribute: dbm_interval -> DBM.bound -> (DBM.bound dbm_constraint) list
end with module DBM=DBM

(** This module aggregates `Value_order` and `Distributor`.
    It is often safer to have both in a single function because of incompatibilities.
    For example, the lower bound value must be bisected with `x <= lb \/ x > lb` instead of `x < lb \/ x >= lb`. *)
module type ValueDistributor = functor (DBM: DBM_sig) ->
sig
  module DBM : DBM_sig
  val distribute: DBM.t -> dbm_interval -> (DBM.bound dbm_constraint) list
end with module DBM=DBM

module type Octagon_split_sig = functor (DBM : DBM_sig) ->
sig
  module DBM : DBM_sig
  val split: DBM.t -> (DBM.bound dbm_constraint) list
end with module DBM=DBM

module Middle : Value_order

module Input_order(Fold_interval: Fold_interval_sig) : Variable_order
module First_fail(Fold_interval: Fold_interval_sig) : Variable_order
module Anti_first_fail(Fold_interval: Fold_interval_sig) : Variable_order
module Min_max : Variable_order

module Bisect_middle : ValueDistributor
module Assign_LB : ValueDistributor
module Assign_UB : ValueDistributor

module Right_to_left(DISTRIBUTOR: ValueDistributor) : ValueDistributor

module Make
  (VARIABLE: Variable_order)
  (VALUE_DISTRIBUTOR: ValueDistributor) : Octagon_split_sig

module First_fail_bisect : Octagon_split_sig
