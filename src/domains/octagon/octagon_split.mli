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
(* Strategy described in (Schutt and al., Solving RCPSP/max by lazy clause generation, 2013).
   Min_LB > Anti_first_fail > Input_order (tried in this order in case of ties between variables). *)
module Min_LB_AFF_IO(Fold_interval: Fold_interval_sig) : Variable_order
module Min_LB(Fold_interval: Fold_interval_sig) : Variable_order
module Min_UB(Fold_interval: Fold_interval_sig) : Variable_order
module Max_LB(Fold_interval: Fold_interval_sig) : Variable_order
module Max_UB(Fold_interval: Fold_interval_sig) : Variable_order
module Max_min : Variable_order
module Min_max : Variable_order

module Bisect_middle : ValueDistributor
module Assign_LB : ValueDistributor
module Assign_UB : ValueDistributor

module Right_to_left(DISTRIBUTOR: ValueDistributor) : ValueDistributor

module Make
  (VARIABLE: Variable_order)
  (VALUE_DISTRIBUTOR: ValueDistributor) : Octagon_split_sig

module First_fail_bisect : Octagon_split_sig
module Min_max_LB : Octagon_split_sig
module Max_min_LB : Octagon_split_sig
module Max_min_UB : Octagon_split_sig
module Max_min_Bisect : Octagon_split_sig
module Max_min_Bisect_reverse : Octagon_split_sig
module First_fail_LB_canonical : Octagon_split_sig
module Anti_first_fail_LB_canonical : Octagon_split_sig
module Anti_first_fail_UB_canonical : Octagon_split_sig
module Anti_first_fail_LB : Octagon_split_sig
module Anti_first_fail_UB : Octagon_split_sig
module MSLF_simple : Octagon_split_sig
module MSLF : Octagon_split_sig
module MSLF_all : Octagon_split_sig
module MSLF_UB : Octagon_split_sig
module MSLF_UB_all : Octagon_split_sig
