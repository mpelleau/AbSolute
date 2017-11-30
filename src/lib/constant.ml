(* type precision = Relative of float | Absolute of float *)
(* let precision       = ref (Absolute(0.001)) *)
let precision       = ref 0.001
let max_iter        = ref 100000000
let max_sol         = ref 10000000
let problem         = ref None
let domain          = ref "box"
let split           = ref "default"
let minimizing      = ref false
let visualization   = ref false
let obj             = ref false
let tex             = ref false
let trace           = ref false
let debug           = ref false
let pruning         = ref false
let sure            = ref false
let iter            = ref false
let pruning_iter    = ref 100000000

(* let parse_prec p = *)
(*   try Scanf.sscanf p "%f%%" (fun f -> *)
(*     if f > 0. then precision := Relative f *)
(*     else failwith "precision must be >= 0% and < 100%") *)
(*   with _ -> *)
(*     try precision := Absolute(float_of_string p) *)
(*     with _ -> failwith "precision must be a float or of the form : x%" *)

let set_prec f =
  if f > 0. then precision := f
  else failwith "precision must be stricly positive"

let set_max_iter i =
  if i > 0 then max_iter := i
  else failwith "number of iterations must be stricly positive"

let set_pruning_iter i =
  if i > 0 then (pruning_iter := i; pruning := true)
  else failwith "number of iterations must be stricly positive"

let set_max_sol s =
  if s > 0 then max_sol := s
  else failwith "number of solutions must be stricly positive"

let set_prob s =
  if Sys.file_exists s then problem := Some s
  else failwith (Format.sprintf "%s : file not found" s)

let set_domain d =
  match d with
  | "box" | "boxS" | "boxCP" | "oct" | "poly"
  | "boxNoct" | "boxNpoly" | "octNpoly" | "BandP" -> domain := d
  | x -> "domain "^x^" undefined" |> failwith

let set_split s =
  match s with
  | "default" | "maxSmear" | "smear" -> split := s
  | x -> "bisection "^x^" undefined" |> failwith
