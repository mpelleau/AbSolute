(******************************************************************)
(* An instance of the solver is parametrized by an abstract domain*)
(* which will be used in the abstract solving process and a       *)
(* rendering module which fits the domain we use                  *)
(******************************************************************)
open Libabsolute
open Signature
open Parser

(** Solve a CSP with the abstract domain Abs *)
module GoS (D : Domain) (P : Propagator) = struct
  module Sol = Solver.Make (D) (P)
  module Show = Out.Make (D)

  let time prob solve =
    Terminal.green_fprintf "\nSolving\n" ;
    Format.printf "domain: " ;
    Terminal.cyan_fprintf "%s\n" !Constant.domain ;
    let time_start = Sys.time () in
    let res = solve !Constant.precision !Constant.max_depth prob in
    let time_end = Sys.time () -. time_start in
    Format.printf "solving time %f\n\n%!" time_end ;
    Terminal.green_fprintf "Results:\n" ;
    res

  let coverage p =
    time p (Sol.coverage ~verbose:true) |> Show.results ~t:!Constant.trace p

  let satisfiability p =
    time p (Sol.satisfiability ~verbose:true) |> Show.satisfiability

  let witness p = time p (Sol.witness ~verbose:true) |> Show.witness
end

(* (\** Solve and minimize a CSP with the abstract domain Abs *\)
 * module GoM (Abs:AbstractCP) = struct
 *   module Min = Minimizer.Minimize(Abs)
 *   module Print = Out.Make(Abs)
 *   let go prob =
 *     let res = Min.minimizing prob in
 *     Print.out_min prob res
 * end *)

(* runs the solver according to the solving mode *)
let run (module D : Domain) (module P : Propagator) (p : Csp.t) : unit =
  let module Solver = GoS (D) (P) in
  if !Constant.trace then Format.printf "\n@[<2>%a@]%!" Csp.print p ;
  if !Constant.witness then Solver.witness p else Solver.coverage p

(* OPTIONS HANDLING *)

let speclist =
  let open Constant in
  let open Argext in
  [ ("-precision", Float set_prec, default_float "Sets the precision" precision)
  ; ( "-max_sol"
    , Int set_max_sol
    , default_int "Sets the maximum number of solutions" max_sol )
  ; ( "-max_iter"
    , Int set_max_iter
    , default_int "Sets the maximum number of iterations" max_iter )
  ; ( "-max_depth"
    , Int set_max_depth
    , default_int "Sets the maximum depth of the search tree" max_iter )
  ; ( "-domain"
    , Set_string domain
    , default_string "Sets the numeric domain" domain )
  ; ( "-iterator"
    , Set_string iterator
    , default_string "Sets the propagation scheme" iterator )
  ; ( "-boolean"
    , Set_string boolean
    , default_string "Sets the boolean domain" domain )
  ; ( "-minimize"
    , Set minimizing
    , "Specify that the problem is a minimization problem" )
  ; ("-iter", Set iter, "Enables the loop for the propagation")
  ; ( "-pruning"
    , Set pruning
    , "Enables the \"pruning\" during the solving process" )
  ; ( "-pruning_iter"
    , Int set_pruning_iter
    , "Changes the number of times the pruning process is applied" )
  ; ( "-split"
    , String set_split
    , options "Changes the splitting strategy used for the solving"
        "default, maxSmear, smear" )
  ; ( "-no-rewrite"
    , Clear rewrite
    , default_bool "Disables the constraint rewriting" rewrite )
  ; ("-debug", Unit set_debug, "Prints the execution for debug purpose")
  ; ( "-debug_lv"
    , Int set_debug_lv
    , "Set the debug level. The higher, most print you get" )
  ; ("-sure", Set sure, "Keeps only the sure solutions")
  ; ("-trace", Set trace, "Prints the solutions on standard output")
  ; ("-visualization", Set visualization, "Enables visualization mode")
  ; ("-witness", Set witness, "Enables witness mode")
  ; ("-obj", Set obj, "Generates an .obj file (for 3D visualization)")
  ; ("-tex", Set tex, "Prints the solutions in latex format on standard output")
  ; ("-svg", Set svg, "Prints the solutions in svg format in  out directory")
  ; ("-pol_as_gen", Set pol_as_gen, "Sets the polyhedra printer to generators")
  ; ("-sbs", Set step_by_step, "Enabling step by step visualization") ]

(*************** ALIASES ************)
let aliases =
  [ ("-p", "-precision")
  ; ("-d", "-domain")
  ; ("-b", "-boolean")
  ; ("-m", "-minimize")
  ; ("-i", "-iter")
  ; ("-pi", "-pruning_iter")
  ; ("-sp", "-split")
  ; ("-s", "-sure")
  ; ("-t", "-trace")
  ; ("-v", "-visualization") ]

let globaldescr =
  "AbSolute is a constraint solver based on abstract domains. For more info, \
   check out https://github.com/mpelleau/AbSolute\n"

let parse_args () =
  let problem = ref "" in
  let set_prob s =
    if Sys.file_exists s then problem := s
    else invalid_arg (Format.sprintf "%s : file not found" s)
  in
  Argext.parse_args_aliases speclist aliases set_prob globaldescr ;
  if !problem = "" then raise (Constant.Error "no filename specified") ;
  Constant.name := !problem ;
  !problem

let parse (fn : string) =
  Format.printf "parsing ... %!" ;
  let p = Parser.file ~check:false fn in
  Format.printf "done.\nsemantic check ... %!" ;
  Parser.semantic_check p ;
  Format.printf "done.\n%!" ;
  p

(* entry point *)
let main () =
  (* for debug *)
  (* Printexc.record_backtrace true ; *)
  Random.init 0x4162536f6c757465 ;
  try
    let prob = parse_args () in
    Terminal.go prob ;
    file prob
    |> run
         (Domains.parse !Constant.domain !Constant.boolean)
         (Domains.iterator ())
  with
  | Constant.Error msg
   |Semantic_error msg
   |Syntax_error msg
   |Lexing_error msg
  ->
    Terminal.error msg ; exit 1
