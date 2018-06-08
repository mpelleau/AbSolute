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

(* built-in instances *)
(* interval domain instance. Only large constraints *)
module SBox       = GoS (Abstract_box.BoxF)(Box_drawer.Make(Abstract_box.BoxF))
module MBox       = GoM (Abstract_box.BoxF)(Box_drawer.Make(Abstract_box.BoxF))

(* interval domain instance. Both large and strict constraints *)
module SBoxStrict = GoS (Abstract_box.BoxStrict)(Realbox_drawer)
module MBoxStrict = GoM (Abstract_box.BoxStrict)(Realbox_drawer)

(* interval domain with rational bounds instance. Only large constraints *)
module SBoxQ      = GoS (Abstract_box.BoxQ)(Box_drawer.Make(Abstract_box.BoxQ))
module MBoxQ      = GoM (Abstract_box.BoxQ)(Box_drawer.Make(Abstract_box.BoxQ))

(* interval domain with rational bounds instance. Both large and strict constraints *)
module SBoxQS     = GoS (Abstract_box.BoxQStrict)(Box_drawer.Make(Abstract_box.BoxQStrict))
module MBoxQS     = GoM (Abstract_box.BoxQStrict)(Box_drawer.Make(Abstract_box.BoxQStrict))


(* apron domain based instances *)
module SBoxCP     = GoS (ADCP.BoxCP)(Apron_drawer.BoxDrawer)
module MBoxCP     = GoM (ADCP.BoxCP)(Apron_drawer.BoxDrawer)
module SOctCP     = GoS (ADCP.OctBoxCP)(Apron_drawer.OctDrawer)
module MOctCP     = GoM (ADCP.OctBoxCP)(Apron_drawer.OctDrawer)
module SPolyCP    = GoS (ADCP.PolyCP)(Apron_drawer.PolyDrawer)
module MPolyCP    = GoM (ADCP.PolyCP)(Apron_drawer.PolyDrawer)

(* reduced product based instances*)
(* module SBoxNOct = GoS (VariousDA.BoxNOct)(Apron_drawer.OctDrawer)
module SBoxNPoly = GoS (VariousDA.BoxNPoly)(Apron_drawer.PolyDrawer)
module SOctNPoly = GoS (VariousDA.OctNPoly)(Apron_drawer.PolyDrawer) *)

module SBoxNOct  = Solver.Solve(VariousDA.BoxNOct)
module SBoxNPoly = Solver.Solve(VariousDA.BoxNPoly)
(*module SBoxNPoly = GoS (VariousDA.BoxNPoly)(Apron_drawer.BoxNPolyDrawer)*)
module SOctNPoly = Solver.Solve(VariousDA.OctNPoly)
module SBoxAndPoly = Solver.Solve(VariousDA.BandP)


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
  if !trace then Format.printf "%a" Csp.print prob;
  if !minimizing then
    match !domain with
    | "box" -> MBox.go prob
    | "boxS" -> MBoxStrict.go prob
    | "boxQ" -> MBoxQ.go prob
    | "boxQS" -> MBoxQS.go prob
    | "boxCP" -> MBoxCP.go prob
    | "oct" -> MOctCP.go prob
    | "poly" -> MPolyCP.go prob
    (*| "boxNoct" -> Minimizer.BoxNOct.minimizing_various prob
    | "boxNpoly" -> Minimizer.BoxNPoly.minimizing_various prob
    | "octNpoly" -> Minimizer.OctNPoly.minimizing_various prob*)
    | _ -> "domain undefined "^(!domain) |> failwith
  else
    match !domain with
    | "box" -> SBox.go prob
    | "boxS" -> SBoxStrict.go prob
    | "boxQ" -> SBoxQ.go prob
    | "boxQS" -> SBoxQS.go prob
    | "boxCP" -> SBoxCP.go prob
    | "oct" -> SOctCP.go prob
    | "poly" -> SPolyCP.go prob
    (* | "boxNoct" -> SBoxNOct.go prob
    | "boxNpoly" -> SBoxNPoly.go prob
    | "octNpoly" -> SOctNPoly.go prob *)
    | "boxNoct" -> SBoxNOct.solving_various prob |> ignore; Format.printf "solving done\n"
    | "boxNpoly" -> SBoxNPoly.solving_various prob|> ignore; Format.printf "solving done\n"
    | "octNpoly" -> SOctNPoly.solving_various prob|> ignore; Format.printf "solving done\n"
    | "BandP" -> SBoxAndPoly.solving_various prob|> ignore; Format.printf "solving done\n"
    | _ -> "domain undefined "^(!domain) |> failwith

let _ = go()
