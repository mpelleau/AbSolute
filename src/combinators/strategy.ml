open Combinator
open Adcp_sig
open State

type combinator =
| Precision
| Propagation
| Solutions_collector
| Elimination
| DFS
| Statistics

type atom =
| Branching

type t =
| A of atom
| C of combinator * t

let propagate_and_search = C (DFS, C (Solutions_collector, C (Propagation, C (Precision, A Branching))))
let propagate_eliminate_and_search = C (DFS, C (Solutions_collector, C (Elimination, C (Propagation, C (Precision, A Branching)))))


let propagate_and_search_stats = C (DFS, C (Statistics, C (Solutions_collector, C (Propagation, C (Precision, A Branching)))))
let propagate_eliminate_and_search_stats = C (DFS, C(Statistics, C (Solutions_collector, C (Elimination, C (Propagation, C (Precision, A Branching))))))

module Make(Abs : AbstractCP) = struct

  module Abs = Abs
  module type Combi = Combinator with type Abs.t = Abs.t

  module type CSig = sig include Combi val init: Abs.t state -> Abs.t state end
  module type PSig = sig include Combi val init: float -> Abs.t state -> Abs.t state end

  let rec make_strategy : t * Abs.t state -> (module Combi) * Abs.t state = function
  | A Branching, state ->
      let (module B: CSig) = (module Brancher.Combine(Abs)) in
      (module B), B.init state
  | C (Precision, sub), state ->
      let (module Sub), state = make_strategy (sub, state) in
      let (module P: PSig) = (module Precision.Combine(Sub)) in
      (module P), P.init !Constant.precision state
  | C (Propagation, sub), state ->
      let (module Sub), state = make_strategy (sub, state) in
      let (module P: CSig) = (module Propagation.Combine(Sub)) in
      (module P), P.init state
  | C (Solutions_collector, sub), state ->
      let (module Sub), state = make_strategy (sub, state) in
      let (module S: CSig) = (module Solutions_collector.Combine(Sub)) in
      (module S), S.init state
  | C (Elimination, sub), state ->
      let (module Sub), state = make_strategy (sub, state) in
      let (module E: CSig) = (module Elimination.Combine(Sub)) in
      (module E), E.init state
  | C (DFS, sub), state ->
      let (module Sub), state = make_strategy (sub, state) in
      let (module D: CSig) = (module Dfs.Combine(Sub)) in
      (module D), D.init state
  | C (Statistics, sub), state ->
      let (module Sub), state = make_strategy (sub, state) in
      let (module S: CSig) = (module Statistics.Combine(Sub)) in
      (module S), S.init state
end
