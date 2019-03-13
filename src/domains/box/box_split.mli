open Var_store

module type Variable_order = functor (S: Var_store_sig) ->
sig
  module Store : Var_store_sig
  val select: Store.t -> (Csp.var * Store.cell) option
end with module Store=S

module type Value_order = functor (I: Itv_sig.ITV) ->
sig
  module I: Itv_sig.ITV
  val select: I.t -> Csp.expr
end with module I=I

module type Distributor =
sig
  val distribute: Csp.var -> Csp.expr -> Csp.bconstraint list
end

module type Box_split_sig = functor (S: Var_store_sig) ->
sig
  module Store : Var_store_sig
  val split: Store.t -> Csp.bconstraint list
end with module Store=S

module Input_order : Variable_order
module First_fail : Variable_order
module Anti_first_fail : Variable_order

module Middle : Value_order
module Lower_bound : Value_order
module Upper_bound : Value_order

module Assign : Distributor
module Bisect : Distributor

module Make
  (Variable: Variable_order)
  (Value: Value_order)
  (Distrib: Distributor) : Box_split_sig

module First_fail_bisect : Box_split_sig
