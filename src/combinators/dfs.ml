open Combinator
open State

(* Depth-first search.
   It terminates whenever it encounters a "Stop" branch or the tree has been fully explored by the sub strategy.
   If search was stopped, then it returns a list with the branch that stopped the search; otherwise an empty list.
*)

module Combine(Sub: Combinator) = struct
  module Abs = Sub.Abs

  let init state = state

  exception Stop_search of Abs.t state

  let search (global, backtrackable) =
    let rec aux global = function
      | Unknown backtrackable ->
          let global, branches = Sub.search (global, backtrackable) in
          List.fold_left aux global branches
      | Stop backtrackable -> raise (Stop_search (global, backtrackable))
      | _ -> global in
    try
      aux global (Unknown backtrackable), []
    with Stop_search (global, backtrackable) ->
      global, [Stop backtrackable]
end
