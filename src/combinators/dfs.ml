open Combinator
open State

module Combine(Sub: Combinator) = struct
  module Abs = Sub.Abs

  let init state = state

  let unknown_branches branches =
    let unwrap_unknown = function
    | Unknown s -> [s]
    | _ -> [] in
    List.flatten (List.map unwrap_unknown branches)

  let search (global, backtrackable) =
    let rec aux global backtrackable =
      let global, branches = Sub.search (global, backtrackable) in
      List.fold_left aux global (unknown_branches branches) in
    aux global backtrackable, []
end
