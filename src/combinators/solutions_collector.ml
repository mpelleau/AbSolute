open Combinator
open Node_status
open State

module Combine(Sub: Combinator) = struct
  module Abs = Sub.Abs

  include Result.Make(Abs)

  type backtrackable = Sub.backtrackable
  type global = Abs.t Result.res * Sub.global
  type state = (backtrackable, Abs.t) State.state

  let init ((subg:Sub.global), (subb:Sub.backtrackable)) =
    (empty_res, subg), subb

  let search (res, subg) state =
    let subg', state = Sub.search subg state in
    let res = match state with
      | Satisfiable state -> add_s res (state.abs, state.constants, state.view)
      | Fail state -> res
      | Prune state -> add_u res (state.abs, state.constants, state.view)
      | unknown -> res
    in
    ((res, subg'), state)
end
