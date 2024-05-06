(** the type of error exception *)
exception Error of string

let name = ref ""

let precision = ref 0.01

let max_iter = ref 100000000

let max_depth = ref 100000000

let max_sol = ref 10000000

let domain = ref "box"

let boolean = ref "boolean"

let iterator = ref "roundrobin"

let product = ref false

let witness = ref false

let split = ref "default"

let join = ref "weak"

let minimizing = ref false

let visualization = ref false

let obj = ref false

let tex = ref false

let svg = ref false

let trace = ref false

let debug = ref 0

let pruning = ref false

let sure = ref false

let iter = ref false

let pruning_iter = ref 100000000

let rewrite = ref true

let step_by_step = ref false

let pol_as_gen = ref false

let set_debug_lv lv =
  if lv >= 0 then debug := lv else raise (Error "debug level must be positive")

let set_debug () = debug := 1

let set_prec f =
  if f > 0. then precision := f
  else raise (Error "precision must be stricly positive")

let set_max_iter i =
  if i > 0 then max_iter := i
  else raise (Error "number of iterations must be stricly positive")

let set_max_depth i =
  if i > 0 then max_depth := i
  else raise (Error "depth must be stricly positive")

let set_pruning_iter i =
  if i > 0 then (
    pruning_iter := i ;
    pruning := true )
  else raise (Error "number of iterations must be stricly positive")

let set_max_sol s =
  if s > 0 then max_sol := s
  else raise (Error "number of solutions must be stricly positive")

let set_split s =
  match s with
  | "default" | "maxSmear" | "smear" | "pizza" -> split := s
  | x -> raise (Error ("bisection " ^ x ^ " undefined"))

let set_iterator s =
  match s with
  | "roundrobin" | "graph" -> iterator := s
  | x -> raise (Error ("iterator " ^ x ^ " undefined"))
