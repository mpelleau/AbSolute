open Var_store
open Csp
open Abstract_domain

module type Box_closure_sig = functor (S: Var_store_sig) ->
sig
  module Store : Var_store_sig
  val incremental_closure: Store.t -> bconstraint -> Store.t
  val entailment: Store.t -> bconstraint -> kleene
end with module Store=S

module Make : Box_closure_sig
