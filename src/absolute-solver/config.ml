(******************************************************************)
(* An instance of the solver is parametrized by an abstract domain*)
(* which will be used in the abstract solving process and a       *)
(* rendering module which fits the domain we use                  *)
(******************************************************************)

open Signature

(** Solve a CSP with the abstract domain Abs *)
module GoS (D : Domain) = struct
  module It = Iterator.Make (D)
  module Solv = Solver.Make (It)
  module Print = Out.Make (D)

  let time_stats prob solve =
    Tools.green_fprintf "\nSolving\n" ;
    Format.printf "domain: " ;
    Tools.cyan_fprintf "%s\n" !Constant.domain ;
    let time_start = Sys.time () in
    let csp = It.init prob in
    let res = solve !Constant.max_depth csp in
    let time_end = Sys.time () -. time_start in
    Format.printf "solving time %f\n\n%!" time_end ;
    Tools.green_fprintf "Results:\n" ;
    res

  let coverage prob =
    let res = time_stats prob Solv.coverage in
    Print.results ~t:!Constant.trace prob res

  let satisfiability prob =
    let res = time_stats prob Solv.satisfiability in
    Print.satisfiability res

  let witness prob =
    let res = time_stats prob Solv.witness in
    Print.witness res
end

(* (\** Solve and minimize a CSP with the abstract domain Abs *\)
 * module GoM (Abs:AbstractCP) = struct
 *   module Min = Minimizer.Minimize(Abs)
 *   module Print = Out.Make(Abs)
 *   let go prob =
 *     let res = Min.minimizing prob in
 *     Print.out_min prob res
 * end *)

(** runs the solver according to the solving mode *)
let run (module D : Domain) (prob : Csp.prog) : unit =
  let module Solver = GoS (D) in
  if !Constant.witness then Solver.witness prob else Solver.coverage prob

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
  ; ( "-lin"
    , String Vpl_domain.set_lin
    , "Sets the linearization algorithm of the VPL" )
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
  Argext.parse_args_aliases speclist aliases Constant.set_prob globaldescr ;
  if !Constant.problem = "" then raise (Constant.Error "no filename specified")
