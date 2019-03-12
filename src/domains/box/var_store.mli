
module type Var_store_sig =
sig
  type t
  type cell

  val add: string -> cell -> t -> t
  val find: string -> t -> cell
  val empty: t
  val filter: (string -> cell -> bool) -> t -> t
  val fold: (string -> cell -> 'b -> 'b) -> t -> 'b -> 'b
  val iter: (string -> cell -> unit) -> t -> unit
  val print: Format.formatter -> t -> unit
end

module Make(I: Itv_sig.ITV) : Var_store_sig with type cell=I.t
