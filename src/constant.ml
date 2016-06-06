let precision       = ref 0.001
let max_iter        = ref 10000000
let max_sol         = ref 1000000
let problem         = ref None
let domain          = ref "box"
let minimizing      = ref false
let visualization   = ref false
let obj             = ref false
let tex             = ref false
let trace           = ref false
let pruning         = ref false

let set_prec p = 
  if p > 0. then precision := p
  else failwith "precision must be stricly positive"

let set_max_iter i = 
  if i > 0 then max_iter := i
  else failwith "number of iterations must be stricly positive"

let set_max_sol s = 
  if s > 0 then max_sol := s
  else failwith "number of solutions must be stricly positive"

let set_prob s =
  if Sys.file_exists s then problem := Some s
  else failwith (Format.sprintf "%s : file not found" s)

let set_domain d = 
  match d with
  | "box" | "boxCP" | "oct" | "poly" | "boxNoct" | "boxNpoly" | "octNpoly" -> domain := d
  | _ -> "domain "^(!domain)^" undefined" |> failwith
