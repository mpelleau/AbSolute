open Dbm

module type Closure_sig =
sig
  module DBM : DBM_sig
  val closure: DBM.t -> unit
  val is_consistent : DBM.t -> unit
end

module ClosureZ(DBM: DBM_sig with type cell = Bound_int.t) : Closure_sig
module ClosureQ(DBM: DBM_sig with type cell = Bound_rat.t) : Closure_sig
module ClosureF(DBM: DBM_sig with type cell = Bound_float.t) : Closure_sig
