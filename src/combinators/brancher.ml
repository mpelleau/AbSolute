open Adcp_sig
open State

module Combine(Abs: AbstractCP) = struct
  module Abs = Abs
  include Splitter.Make(Abs)
  let init state = state
  let search (global, backtrackable) = global, (List.map (unknown backtrackable) (split backtrackable.abs backtrackable.jacobian))
end
