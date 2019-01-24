open Adcp_sig
open Node_status
open State

module Combine(Abs: AbstractCP) = struct
  module Abs = Abs
  type global = unit
  type backtrackable = unit
  type state = (backtrackable, Abs.t) State.state
  include Splitter.Make(Abs)
  let init () = (), ()
  let search global state = global, (List.map (unknown state) (split state.abs state.constraints))
end