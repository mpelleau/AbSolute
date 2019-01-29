open Combinator
open State

module Combine(Sub: Combinator) = struct
  module Abs = Sub.Abs

  include Splitter.Make(Abs)

  let init state = state

  let search (global, backtrackable) =
    match consistency backtrackable.abs backtrackable.jacobian backtrackable.constants with
    | Empty -> global, [Fail backtrackable]
    | Full (abs', const) -> global, [Satisfiable {backtrackable with abs=abs'; constants=const}]
    | Maybe(abs', ctrs', csts') -> Sub.search (global, {backtrackable with abs=abs'; jacobian=ctrs'; constants=csts'})
end
