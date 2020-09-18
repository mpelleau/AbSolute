(******************************************************************)
(*                   entry point of the solver                    *)
(******************************************************************)

(******************************************************************)
(* An instance of the solver is parametrized by an abstract domain*)
(* which will be used in the abstract solving process and a       *)
(* rendering module which fits the domain we use                  *)
(******************************************************************)

open Signature

(** Solve a CSP with the abstract domain Abs *)
module GoS (Abs:AbstractCP) = struct
  module Sol = Solver.Solve(Abs)
  module Print = Out.Make(Abs)
  let go prob =
    Format.printf "Starting the resolution using the ";
    Tools.green_fprintf Format.std_formatter "%s" (!Constant.domain);
    Format.printf " domain ...\n%!";
    let res = Sol.solving prob in
    Format.printf "Solving finished\n\n%!";
    Print.out prob res
end

(** Solve and minimize a CSP with the abstract domain Abs *)
module GoM (Abs:AbstractCP) = struct
  module Min = Minimizer.Minimize(Abs)
  module Print = Out.Make(Abs)
  let go prob =
    let res = Min.minimizing prob in
    Print.out_min prob res
end

(***************)
(*   domains   *)
(***************)

module type FullDomain = sig
  module Abstract : AbstractCP
end

module MakeFullDomain
         (Abstract : AbstractCP)
  = struct
  module Abstract = Abstract
end

module MakeProduct (D1 : FullDomain) (D2 : FullDomain) = struct
  module Abstract = Product.MakeProduct(D1.Abstract)(D2.Abstract)
end

let get_domain : string -> (module FullDomain) = function
  | "box"    -> (module MakeFullDomain (Abstract_box.BoxF))
  | "boxS"   -> (module MakeFullDomain (Abstract_box.BoxStrict))
  | "boxMix" -> (module MakeFullDomain (Abstract_box.BoxMix))
  | "boxQ"   -> (module MakeFullDomain (Abstract_box.BoxQ))
  | "boxQS"  -> (module MakeFullDomain (Abstract_box.BoxQStrict))
  | "boxCP"  -> (module MakeFullDomain (ADCP.BoxCP))
  | "oct"    -> (module MakeFullDomain (ADCP.OctBoxCP))
  | "poly"   -> (module MakeFullDomain (ADCP.PolyCP))
  | "vpl"    -> (module MakeFullDomain (Vpl_domain.VplCP))
  | s        -> Tools.fail_fmt "Domain %s does not exist" s

let set_domain_from_names : string list -> (module FullDomain)
  = fun names ->
  List.fold_left
    (fun (module D : FullDomain) name ->
      let (module F : FullDomain) = get_domain name in
      let module P = MakeProduct (D)(F) in
      (module P)
    )
    (List.hd names |> get_domain)
    (List.tl names)

let set_domain : unit -> (module FullDomain)
  = fun () ->
  String.split_on_char ',' !Constant.domain
  |> set_domain_from_names

(**
 * Lifts the given abstract domain and its associated drawer into a runnable domain.
 * The results depends on the value of flags {!val:Constant.minimizing} and {!val:Constant.step_by_step}.
 *)
let lift (module D : FullDomain) (prob : Csp.prog) : unit =
  if !Constant.minimizing
  then let module Minimizer = GoM (D.Abstract) in
       Minimizer.go prob
  else
    (* if !Constant.step_by_step
     * then let module SBS = Step_by_step.Make (D.Abstract) in
     *      SBS.solving prob else *)
    let module Solver = GoS (D.Abstract) in
    Solver.go prob

(********************)
(* OPTIONS HANDLING *)
(********************)

let speclist =
  let open Constant in
  let open Argext in
  [
    ("-precision"    , Float set_prec       , default_float "Sets the precision" precision);
    ("-max_sol"      , Int set_max_sol      , default_int "Sets the maximum number of solutions" max_sol);
    ("-max_iter"     , Int set_max_iter     , default_int "Sets the maximum number of iterations" max_iter);
    ("-domain"       , Set_string domain    , options (default_string "Changes the domain used for the solving" domain) "box, boxS, boxQ, boxQS, boxCP, oct, poly, vpl or any combination of them separated by commas (e.g. box,poly,boxQS) ");
    ("-minimize"     , Set minimizing       , "Specify that the problem is a minimization problem");
    ("-iter"         , Set iter             , "Enables the loop for the propagation");
    ("-pruning"      , Set pruning          , "Enables the \"pruning\" during the solving process");
    ("-pruning_iter" , Int set_pruning_iter , "Changes the number of times the pruning process is applied");
    ("-split"        , String set_split     , options "Changes the splitting strategy used for the solving" "default, maxSmear, smear");
    ("-lin"          , String Vpl_domain.set_lin      , "Sets the linearization algorithm of the VPL");
    ("-no-rewrite"   , Clear rewrite        , default_bool "Disables the constraint rewriting" rewrite);
    ("-debug"        , Unit set_debug       , "Prints the execution for debug purpose");
    ("-debug_lv"     , Int set_debug_lv     , "Set the debug level. The higher, most print you get");
    ("-sure"         , Set sure             , "Keeps only the sure solutions");
    ("-trace"        , Set trace            , "Prints the solutions on standard output");
    ("-visualization", Set visualization    , "Enables visualization mode");
    ("-obj"          , Set obj              , "Generates an .obj file (for 3D visualization)");
    ("-tex"          , Set tex              , "Prints the solutions in latex format on standard output");
    ("-sbs"          , Set step_by_step     , "Enabling step by step visualization");
  ]

(*************** ALIASES ************)
let aliases =
  [
    ("-p", "-precision");
    ("-d", "-domain");
    ("-m", "-minimize");
    ("-i", "-iter");
    ("-pi","-pruning_iter");
    ("-sp","-split");
    ("-s", "-sure");
    ("-t", "-trace");
    ("-v", "-visualization");
  ]

let globaldescr =
  "AbSolute is a constraint solver based on abstract domains. For more info, check out https://github.com/mpelleau/AbSolute\n"

let parse_args () =
  Argext.parse_args_aliases speclist aliases Constant.set_prob globaldescr;
  if !Constant.problem = "" then raise (Constant.Error "no filename specified")
