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

(* reduced product based instances*)
(*
module SBoxNOct  = Solver.Solve(VariousDA.BoxNOct)
module SBoxNPoly = Solver.Solve(VariousDA.BoxNPoly)
(*module SBoxNPoly = GoS (VariousDA.BoxNPoly)(Apron_drawer.BoxNPolyDrawer)*)
module SOctNPoly = Solver.Solve(VariousDA.OctNPoly)
module SBoxAndPoly = Solver.Solve(VariousDA.BandP)
*)

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

module Test = GoS(Vpl_domain.VplCP) (Vpl_drawer)

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
  ("-domain"       , String set_domain    , default_string "Changes the domain used for the solving" domain);
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
  ("-split"        , String set_split     , "Changes the splitting strategy used for the solving");
  ("-no-rewrite"   , Clear rewrite        , default_bool "Disables the constraint rewriting" rewrite);
  ("-sbs"          , Set step_by_step     , "Enabling step by step visualization");
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
  if !trace then Format.printf "%a" Csp.print prob;
  match !domain with
    | "box" -> lift (module Abstract_box.BoxF) (module Box_drawer.Make(Abstract_box.BoxF)) prob
    | "boxS" -> lift (module Abstract_box.BoxStrict) (module Realbox_drawer) prob
    | "boxQ" -> lift (module Abstract_box.BoxQ) (module Box_drawer.Make(Abstract_box.BoxQ)) prob
    | "boxQS" -> lift (module Abstract_box.BoxQStrict) (module Box_drawer.Make(Abstract_box.BoxQStrict)) prob
    | "boxCP" -> lift (module ADCP.BoxCP) (module Apron_drawer.BoxDrawer) prob
    | "oct" -> lift (module ADCP.OctBoxCP) (module Apron_drawer.OctDrawer) prob
    | "poly" -> lift (module ADCP.PolyCP) (module Apron_drawer.PolyDrawer) prob
    | _ -> "domain undefined "^(!domain) |> failwith
    (* TODO : fix produit réduit
    | "boxNoct" -> lift (module VariousDA.BoxNOct) (module Apron_drawer.OctDrawer) prob
    | "boxNpoly" -> lift (module VariousDA.BoxNPoly) (module Apron_drawer.PolyDrawer) prob
    | "octNpoly" -> lift (module VariousDA.OctNPoly) (module Apron_drawer.PolyDrawer) prob
    | "boxNoct" -> SBoxNOct.solving_various prob |> ignore; Format.printf "solving done\n"
    | "boxNpoly" -> SBoxNPoly.solving_various prob|> ignore; Format.printf "solving done\n"
    | "octNpoly" -> SOctNPoly.solving_various prob|> ignore; Format.printf "solving done\n"
    | "BandP" -> SBoxAndPoly.solving_various prob|> ignore; Format.printf "solving done\n"
    | _ -> "domain undefined "^(!domain) |> failwith
    *)

let _ = go()
