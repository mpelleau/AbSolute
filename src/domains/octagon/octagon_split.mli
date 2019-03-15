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

module type Octagon_split_sig = functor (DBM : DBM_sig) ->
sig
  module DBM : DBM_sig
  val split: DBM.t -> (DBM.bound dbm_constraint) list
end with module DBM=DBM

module Input_order(Fold_interval: Fold_interval_sig) : Variable_order
module First_fail(Fold_interval: Fold_interval_sig) : Variable_order
module Anti_first_fail(Fold_interval: Fold_interval_sig) : Variable_order
module Min_max : Variable_order

module Middle : Value_order
module Lower_bound : Value_order
module Upper_bound : Value_order

module Bisect : Distributor

module Make
  (VARIABLE: Variable_order)
  (VALUE: Value_order)
  (DISTRIBUTOR: Distributor) : Octagon_split_sig
