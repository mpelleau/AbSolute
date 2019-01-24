open Combinator
open Node_status

module Combine(Sub: Combinator) = struct
  module Abs = Sub.Abs
  type backtrackable = Sub.backtrackable
  type global = Sub.global
  type state = (backtrackable, Abs.t) State.state

  let init sub = sub

  let rec search global state =
    match Sub.search global state with
    | global, Unknown child_nodes ->
        List.fold_left search global child_nodes
    | global, _ -> global
end
