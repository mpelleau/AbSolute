
module type Var_store_sig =
sig
  type t
  type cell

  val add: string -> cell -> t -> t
  val find: string -> t -> cell
  val empty: t
  val filter: (string -> cell -> bool) -> t -> t
  val fold: (string -> cell -> 'b -> 'b) -> t -> 'b -> 'b
end

module Make(I: Itv_sig.ITV) =
struct
  type cell = I.t
  module Env = Tools.VarMap
  type t = cell Env.t
  let add = Env.add
  let find = Env.find_fail
  let empty = Env.empty
  let filter = Env.filter
  let fold = Env.fold
end
