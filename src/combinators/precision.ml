open Combinator
open Node_status
open State

module Combine(Sub: Combinator) = struct
  module Abs = Sub.Abs
  type backtrackable = Sub.backtrackable
  type global = float * Sub.global
  type state = (backtrackable, Abs.t) State.state

  let init prec ((subg:Sub.global), (subb:Sub.backtrackable)) =
    (prec, subg), subb

  let search global state =
    if Abs.is_small state.abs then
      global, [Prune state]
    else
      let (prec, subg) = global in
      let (subg, state) = Sub.search subg state in
      (prec, subg), state
end
