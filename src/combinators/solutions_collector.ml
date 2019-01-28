open Combinator
open State

module Combine(Sub: Combinator) = struct
  module Abs = Sub.Abs

  include Result.Make(Abs)

  let init (global, backtrackable) =
    {global with res=Some(empty_res)}, backtrackable

  let collect_branch res branch =
     match branch with
    | Satisfiable backtrackable -> add_s res (backtrackable.abs, backtrackable.constants, backtrackable.view)
    | Fail backtrackable -> res
    | Prune backtrackable -> add_u res (backtrackable.abs, backtrackable.constants, backtrackable.view)
    | Unknown _ -> res
    | Stop _ -> res

  let search state =
    let (global, branches) = Sub.search state in
    let res = List.fold_left collect_branch (res global) branches in
    {global with res=Some(res)}, branches
end
