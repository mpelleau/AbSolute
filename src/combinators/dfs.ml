open Combinator
open Node_status

module Combine(Sub: Combinator) = struct
  module Abs = Sub.Abs
  type backtrackable = Sub.backtrackable
  type global = Sub.global
  type state = (backtrackable, Abs.t) State.state

  let init sub = sub

  let unknown_branches branches =
    let unwrap_unknown = function
    | Unknown s -> [s]
    | _ -> [] in
    List.flatten (List.map unwrap_unknown branches)

  let rec search global state =
    let global, branches = Sub.search global state in
    List.fold_left search global (unknown_branches branches)
end
