type 'a node_status =
  | Satisfiable of 'a
  | Fail of 'a
  | Prune of 'a
  | Unknown of 'a list
