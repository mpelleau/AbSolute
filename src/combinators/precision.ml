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

  let lift prec sub = (prec, sub)

  let search this state =
    if Abs.is_small state.abs then
      this, Prune state
    else
      let (prec, sub) = this in
      let (sub, state) = Sub.search sub state in
      (lift prec sub, state)
end
