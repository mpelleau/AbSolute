(******************************************************************)
(*                   entry point of the solver                    *)
(******************************************************************)

(******************************************************************)
(* An instance of the solver is parmetrized by an abstract domain *)
(* which will be used in the abstract solving process and a       *)
(* rendering module witch fits the domain we use                  *)
(******************************************************************)

open Drawer_sig

module GoS (Abs:Adcp_sig.AbstractCP)(Dr:Drawer with type t = Abs.t) = struct
  module Sol = Solver.Solve(Abs)
  module Print = Out.Make(Dr)
  let go prob =
    let res = Sol.solving prob in
    Print.out prob res
end

module GoM (Abs:Adcp_sig.AbstractCP)(Dr:Drawer with type t = Abs.t) = struct
  module Min = Minimizer.Minimize(Abs)
  module Print = Out.Make(Dr)
  let go prob =
    let res = Min.minimizing prob in
    Print.out_min prob res
end

(************************)
(* THE SOLVER INSTANCES *)
(************************)

(**
 * Lifts the given abstract domain and its associated drawer into a runnable domain.
 * The results depends on the value of flags {!val:Constant.minimizing} and {!val:Constant.step_by_step}.
 *)
let lift (type s) (module Domain : Adcp_sig.AbstractCP with type t = s) (module Drawer : Drawer_sig.Drawer with type t = s) (prob : Csp.prog) : unit =
    if !Constant.minimizing
    then let module Minimizer = GoM (Domain)(Drawer) in
        Minimizer.go prob
    else
        if !Constant.step_by_step
        then let module SBS = Step_by_step.Make (Domain)(Drawer) in
            SBS.solving prob
        else let module Solver = GoS (Domain)(Drawer) in
             Solver.go prob

(********************)
(* OPTIONS HANDLING *)
(********************)

let speclist =
  let open Constant in
  let open Argext in
  [
  ("-visualization", Set visualization    , "Enables visualization mode");
  ("-precision"    , Float set_prec       , default_float "Sets the precision" precision);
  ("-max_sol"      , Int set_max_sol      , default_int "Sets the maximum number of solutions" max_sol);
  ("-max_iter"     , Int set_max_iter     , default_int "Sets the maximum number of iterations" max_iter);
  ("-domain"       , String set_domain    , options (default_string "Changes the domain used for the solving" domain) "box, boxS, boxQ, boxQS, boxCP, oct, poly, boxNoct, boxNpoly, octNpoly, BandP, vpl");
  ("-obj"          , Set obj              , "Generates an .obj file (for 3D visualization)");
  ("-tex"          , Set tex              , "Prints the solutions in latex format on stadard output");
  ("-pruning"      , Set pruning          , "Enables the \"pruning\" during the solving process");
  ("-trace"        , Set trace            , "Prints the solutions on standard output");
  ("-sure"         , Set sure             , "Keeps only the sure solutions");
  ("-minimize"     , Set minimizing       , "Specify that the problem is a minimization problem");
  ("-iter"         , Set iter             , "Enables the loop for the propagation");
  ("-pruning_iter" , Int set_pruning_iter , "Changes the number of times the pruning process is applied");
  ("-debug"        , Unit set_debug       , "Prints the execution for debug purpose");
  ("-debug_lv"     , Int set_debug_lv     , "Set the debug level. The higher, most print you get");
  ("-split"        , String set_split     , options "Changes the splitting strategy used for the solving" "default, maxSmear, smear");
  ("-no-rewrite"   , Clear rewrite        , default_bool "Disables the constraint rewriting" rewrite);
  ("-sbs"          , Set step_by_step     , "Enabling step by step visualization");
  ("-lin"          , Arg.String Vpl_domain.set_lin      , "Sets the linearization algorithm of the VPL");
  ("-vpl_split"          , Arg.String Vpl_domain.set_split      , "Sets the split strategy of the VPL");
]

(*************** ALIASES ************)
let aliases =
  [
  ("-m", "-minimize");
  ("-t", "-trace");
  ("-s", "-sure");
  ("-v", "-visualization");
  ("-p", "-precision");
  ("-d", "-domain");
  ("-i", "-iter");
  ("-pi","-pruning_iter");
  ("-sp","-split");
  ]

let globaldescr =
  "AbSolute is a constraint solver based on abstract domains. For more info, check out https://github.com/mpelleau/AbSolute\n"

let parse_args () = Argext.parse_args_aliases speclist aliases Constant.set_prob globaldescr

(***************)
(* entry point *)
(***************)

let go() =
  let open Constant in
  parse_args ();
  Format.printf "domain : %s\n" !domain;
  let prob = File_parser.parse !problem in
  Format.printf "file parsed\n";
  if !debug > 0 then Vpl_domain.enable_debug();
  if !trace then Format.printf "%a" Csp.print prob;
  match !domain with
    | "box" -> lift (module Abstract_box.BoxF) (module Box_drawer.Make(Abstract_box.BoxF)) prob
    | "boxS" -> lift (module Abstract_box.BoxStrict) (module Realbox_drawer) prob
    | "boxQ" -> lift (module Abstract_box.BoxQ) (module Box_drawer.Make(Abstract_box.BoxQ)) prob
    | "boxQS" -> lift (module Abstract_box.BoxQStrict) (module Box_drawer.Make(Abstract_box.BoxQStrict)) prob
    | "boxCP" -> lift (module ADCP.BoxCP) (module Apron_drawer.BoxDrawer) prob
    | "oct" -> lift (module ADCP.OctBoxCP) (module Apron_drawer.OctDrawer) prob
    | "poly" -> lift (module ADCP.PolyCP) (module Apron_drawer.PolyDrawer) prob
    | "vpl" -> lift (module Vpl_domain.VplCP) (module Vpl_drawer) prob
    | "boxNoct" -> lift (module VariousDA.BoxNOct) (module VariousDA_drawer.BoxNoctDrawer) prob
    | "boxNpoly" -> lift (module VariousDA.BoxNPoly) (module VariousDA_drawer.BoxNpolyDrawer) prob
    | "octNpoly" -> lift (module VariousDA.OctNPoly) (module VariousDA_drawer.OctNpolyDrawer) prob
    | "BandP" -> lift (module VariousDA.BandP) (module VariousDA_drawer.BandPDrawer) prob
    | _ -> "domain undefined "^(!domain) |> failwith
    (* TODO : fix produit réduit
    | "boxNoct" -> SBoxNOct.solving_various prob |> ignore; Format.printf "solving done\n"
    | "boxNpoly" -> SBoxNPoly.solving_various prob|> ignore; Format.printf "solving done\n"
    | "octNpoly" -> SOctNPoly.solving_various prob|> ignore; Format.printf "solving done\n"
    | _ -> "domain undefined "^(!domain) |> failwith
    *)

let _ = go()
