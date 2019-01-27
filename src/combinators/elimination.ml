open Combinator
open State

module Combine(Sub: Combinator) = struct
  module Abs = Sub.Abs

  include Splitter.Make(Abs)

  let init state = state

  let prune branch =
    match branch with
    | Unknown state ->
        let satisfiable_abs,unknown_abs = prune state.abs state.constraints in
        let satisfiable_space = List.map (satisfiable state) satisfiable_abs in
        let unknown_space = List.map (unknown state) unknown_abs in
        satisfiable_space@unknown_space
    | branch -> [branch]

  let search state =
    let global, branches = Sub.search state in
    global, List.flatten (List.map prune branches)
end
