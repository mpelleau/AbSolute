type 'a backtrackable = {
  abs: 'a;
  constraints: Csp.ctrs;
  constants: Csp.csts;
  view: Csp.jacob;
}

(* Global state proper to each strategy. *)
type 'a global = {
  precision: float option;
  res: ('a Result.res) option;
}

type 'a state = 'a global * 'a backtrackable

let init abs constraints constants view =
  {precision=None;
   res=None},
  {abs=abs;
   constraints=constraints;
   constants=constants;
   view=view}

exception Field_not_found of string

let precision global =
  match global.precision with
  | Some(p) -> p
  | None -> raise (Field_not_found "precision")

let res global =
  match global.res with
  | Some(r) -> r
  | None -> raise (Field_not_found "res")

type 'a branch_kind =
  | Satisfiable of 'a backtrackable
  | Fail of 'a backtrackable
  | Prune of 'a backtrackable
  | Unknown of 'a backtrackable

type 'a branches = 'a global * 'a branch_kind list

let satisfiable backtrackable' abs = Satisfiable {backtrackable' with abs=abs}
let unknown backtrackable' abs = Unknown {backtrackable' with abs=abs}
