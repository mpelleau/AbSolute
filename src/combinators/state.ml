
type global_statistics = {
  start: Mtime_clock.counter;
  elapsed: Mtime.span;
  nodes: int;
  fails: int;
  sols: int;
  pruned: int;
  depth_max: int;
}

type backtrackable_statistics = {
  phantom: int;
  depth: int
}

type precision = float
type 'a collect_solutions = 'a Result.res
type timeout_bound = Mtime.span
type max_solutions_bound = int

type 'a backtrackable = {
  abs: 'a;
  constraints: Csp.constrs;
  jacobian: Csp.ctrs;
  constants: Csp.csts;
  view: Csp.jacob;
  bt_stats: backtrackable_statistics option
}

(* Global state proper to each strategy. *)
type 'a global = {
  precision: precision option;
  solutions: ('a collect_solutions) option;
  statistics: global_statistics option;
  timeout: timeout_bound option;
  max_solutions: max_solutions_bound option
}

type 'a state = 'a global * 'a backtrackable

let init abs constraints constants view =
  let jacobian = List.map (fun c -> (c,[])) constraints in
  {precision=None;
   solutions=None;
   statistics=None;
   timeout=None;
   max_solutions=None},
  {abs=abs;
   constraints=constraints;
   jacobian=jacobian;
   constants=constants;
   view=view;
   bt_stats=None}

exception Field_not_found of string

let precision global =
  match global.precision with
  | Some(p) -> p
  | None -> raise (Field_not_found "precision")

let solutions global =
  match global.solutions with
  | Some(r) -> r
  | None -> raise (Field_not_found "solutions")

let statistics s =
  match s.statistics with
  | Some(s) -> s
  | None -> raise (Field_not_found "statistics")

let bt_stats s =
  match s.bt_stats with
  | Some(s) -> s
  | None -> raise (Field_not_found "bt_stats")

let timeout global =
  match global.timeout with
  | Some(t) -> t
  | None -> raise (Field_not_found "timeout")

let max_solutions global =
  match global.max_solutions with
  | Some(s) -> s
  | None -> raise (Field_not_found "max_solutions")

type 'a branch_kind =
  | Satisfiable of 'a backtrackable
  | Fail of 'a backtrackable
  | Prune of 'a backtrackable
  | Unknown of 'a backtrackable
  | Stop of 'a backtrackable

type 'a branches = 'a global * 'a branch_kind list

let satisfiable backtrackable' abs = Satisfiable {backtrackable' with abs=abs}
let unknown backtrackable' abs = Unknown {backtrackable' with abs=abs}
