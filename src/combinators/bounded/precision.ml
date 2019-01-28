open Combinator
open State

module Combine(Sub: Combinator) = struct
  module Abs = Sub.Abs

  let init precision (global, backtrackable) =
    {global with precision=Some(precision)}, backtrackable

  (* TODO: Precision is not used yet, it should be taken into account by `Abs.is_small`. *)
  let search (global, backtrackable) =
    if Abs.is_small backtrackable.abs then
      global, [Prune backtrackable]
    else
      Sub.search (global, backtrackable)
end
