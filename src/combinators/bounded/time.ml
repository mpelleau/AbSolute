open Combinator
open State

module Combine(Sub: Combinator) = struct
  module Abs = Sub.Abs

  let init timeout (global, backtrackable) =
    {global with timeout=Some(timeout)}, backtrackable

  let search (global, backtrackable) =
    let (global, branches) = Sub.search (global, backtrackable) in
    let timeout = (timeout global) in
    let elapsed = (statistics global).elapsed in
    if Mtime.Span.compare timeout elapsed <= 0 then
      global, [Stop backtrackable]@branches
    else
      global, branches
end
