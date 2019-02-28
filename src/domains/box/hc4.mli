open Var_store
open Csp
open Abstract_domain

module type Box_closure_sig =
sig
  module Store : Var_store_sig
  val closure: Store.t -> bconstraint -> Store.t
  val entailment: Store.t -> bconstraint -> kleene
end

module Make(Store: Var_store_sig) : Box_closure_sig with module Store=Store
