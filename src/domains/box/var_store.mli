
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

module type Var_store_functor = functor (I: Itv_sig.ITV) -> Var_store_sig with module I=I

module Make : Var_store_functor
