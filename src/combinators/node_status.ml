open State

type 'a branch_kind =
  | Satisfiable of 'a
  | Fail of 'a
  | Prune of 'a
  | Unknown of 'a

type 'a branches = 'a branch_kind list

let satisfiable state' abs = Satisfiable {state' with abs=abs}
let unknown state' abs = Unknown {state' with abs=abs}
