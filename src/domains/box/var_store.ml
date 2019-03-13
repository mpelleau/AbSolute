
module type Var_store_sig =
sig
  type t
  module I: Itv_sig.ITV
  type cell=I.t

  val add: string -> cell -> t -> t
  val find: string -> t -> cell
  val empty: t
  val filter: (string -> cell -> bool) -> t -> t
  val fold: (string -> cell -> 'b -> 'b) -> t -> 'b -> 'b
  val iter: (string -> cell -> unit) -> t -> unit
  val print: Format.formatter -> t -> unit
end

module Make(I: Itv_sig.ITV) =
struct
  module I = I
  type cell = I.t
  module Env = Tools.VarMap
  type t = cell Env.t
  let add = Env.add
  let find = Env.find_fail
  let empty = Env.empty
  let filter = Env.filter
  let fold = Env.fold
  let iter = Env.iter
  let print fmt store =
    Env.iter (fun v i -> Format.fprintf fmt "%s=%a \n" v I.print i) store
end
