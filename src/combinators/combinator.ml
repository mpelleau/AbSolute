open State

module type Combinator = sig
  module Abs : Adcp_sig.AbstractCP

  val search: Abs.t state -> Abs.t branches
end
