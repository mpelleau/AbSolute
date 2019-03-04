open Combinator
open State

(* Statistics are updated in each node, after `Sub.search` has been executed.
   The fail/satisfiable/pruned counters are only increased for leaf nodes (those without unknown children nodes).
   (1) A node is failed if it is not pruned nor satisfiable.
   (2) Otherwise it is satisfiable if it is not pruned.
   (3) Otherwise it is pruned.

  The number of internal nodes is equal to "nodes - (fails + solutions + pruned)".
*)
module Combine(Sub: Combinator) = struct
  module Abs = Sub.Abs

  let init (global, backtrackable) =
    let statistics = init_global_stats () in
    {global with statistics=Some(statistics)},
    {backtrackable with bt_stats=Some({depth=0; phantom=0})}

  let decompose_branches branches =
    let increase_counters (fails, sats, pruned) = function
      | Satisfiable _ -> (fails, sats + 1, pruned)
      | Fail _ ->  (fails + 1, sats, pruned)
      | Prune _ -> (fails, sats, pruned + 1)
      | Unknown _
      | Stop _ -> (fails, sats, pruned)
    in
    List.fold_left increase_counters (0,0,0) branches

  let increase_depth branches =
    let inc = function
      | Unknown bt ->
          let stats = (bt_stats bt) in
          let stats = {stats with depth=stats.depth + 1} in
          Unknown {bt with bt_stats=Some(stats)}
      | other -> other in
    List.map inc branches

  let search (global, backtrackable) =
    let (global, branches) = Sub.search (global, backtrackable) in
    let stats = (statistics global) in
    let (fails, sols, pruned) = decompose_branches branches in
    let to_int b = if b then 1 else 0 in
    let stats = {stats with
      elapsed = Mtime_clock.count stats.start;
      nodes = stats.nodes + 1;
      fails = stats.fails + to_int (sols = 0 && pruned = 0 && fails > 0);
      sols = stats.sols + to_int (pruned = 0 && sols > 0);
      pruned = stats.pruned + to_int (pruned > 0);
      depth_max = max stats.depth_max ((bt_stats backtrackable).depth + 1)
    } in
    let branches = increase_depth branches in
    {global with statistics=Some(stats)}, branches
end

let print_stats stats =
  Printf.printf
    "Statistics:\n\
     \tTime: %fs\n\
     \tNodes: %d\n\
     \tFails: %d\n\
     \tSolutions: %d\n\
     \tPruned: %d\n\
     \tDepth max: %d\n"
  (Mtime.Span.to_s stats.elapsed) stats.nodes stats.fails
  stats.sols stats.pruned stats.depth_max
