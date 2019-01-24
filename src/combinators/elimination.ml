open Combinator
open Node_status
open State

module Combine(Sub: Combinator) = struct
  module Abs = Sub.Abs
  type backtrackable = Sub.backtrackable
  type global = Sub.global
  type state = (backtrackable, Abs.t) State.state

  include Splitter.Make(Abs)

  let init sub = sub

  let search global state =
    let satisfiable_abs,unknown_abs = prune state.abs state.constraints in
    let satisfiable_space = List.map (satisfiable state) satisfiable_abs in
    let unknown_space = List.map (unknown state) unknown_abs in
    global, satisfiable_space@unknown_space
end
