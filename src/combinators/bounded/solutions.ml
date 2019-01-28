open Combinator
open State

module Combine(Sub: Combinator) = struct
  module Abs = Sub.Abs

  let init max_solutions (global, backtrackable) =
    {global with max_solutions=Some(max_solutions)}, backtrackable

  let search (global, backtrackable) =
    let (global, branches) = Sub.search (global, backtrackable) in
    let max_solutions = (max_solutions global) in
    let solutions = (statistics global).solutions in
    if solutions >= max_solutions then
      global, [Stop backtrackable]@branches
    else
      global, branches
end
