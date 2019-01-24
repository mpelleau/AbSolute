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
    match consistency state.abs state.constraints state.constants with
    | Empty -> global, Fail state
    | Full (abs', const) -> global, Satisfiable {state with abs=abs'; constants=const}
    | Maybe(abs', ctrs', csts') -> Sub.search global {state with abs=abs'; constraints=ctrs'; constants=csts'}
end
