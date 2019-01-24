open Node_status

module type Combinator = sig
  module Abs : Adcp_sig.AbstractCP
  type global
  type backtrackable
  type state = (backtrackable, Abs.t) State.state

  val search: global -> state -> (global * state node_status)
end
