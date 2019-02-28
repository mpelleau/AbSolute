open Var_store
open Csp
open Abstract_domain

module type Box_closure_sig =
sig
  module Store : Var_store_sig
  val closure: Store.t -> bconstraint -> Store.t
  val entailment: Store.t -> bconstraint -> kleene
end

module Make
  (I: Itv_sig.ITV)
  (Store: Var_store_sig with type cell=I.t) : Box_closure_sig with module Store=Store
