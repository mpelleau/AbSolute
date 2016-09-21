open Drawer_sig

module GoS (Abs:Adcp_sig.AbstractCP)(Dr:Drawer with type t = Abs.t) = struct
  module Sol = Solver.Solve(Abs)
  module Print = Out.Make(Dr)
  let go prob =
    let res = Sol.solving prob in
    Print.out prob res
end

(* module SBox = GoS (Abstract_box.BoxStrict)(Realbox_drawer) *)
module SBox = GoS (Abstract_box.BoxF)(Box_drawer)
(* module SBoxCP    = GoS (ADCP.BoxCP) *)
module SOctCP    = GoS (ADCP.OctBoxCP)(Apron_drawer.OctDrawer)
module SPolyCP   = GoS (ADCP.PolyCP)(Apron_drawer.PolyDrawer)
(* module SBoxNOct  = GoS (VariousDA.BoxNOct) *)
(* module SBoxNPoly = GoS (VariousDA.BoxNPoly) *)
(* module SOctNPoly = GoS (VariousDA.OctNPoly) *)


let speclist =
  let open Constant in
  [
  ("-visualization", Arg.Set visualization, "Enables visualization mode");
  ("-precision"    , Arg.Float set_prec   , "Changes the precision. default is 1e-3");
  ("-max_sol"      , Arg.Int set_max_sol  , "Changes the maximum number of solutions. default is 1e6");
  ("-max_iter"     , Arg.Int set_max_iter , "Changes the maximum number of iterations. default is 1e7");
  ("-domain"       , Arg.String set_domain, "Changes the domain used for the solving. default is box");
  ("-obj"          , Arg.Set obj          , "Generates an .obj file (for 3D visualization)");
  ("-tex"          , Arg.Set tex          , "Prints the solutions in latex format on stadard output");
  ("-pruning"      , Arg.Set pruning      , "Enables the \"pruning\" during the solving process");
  ("-trace"        , Arg.Set trace        , "Prints the solutions on standard output");
  ("-sure"         , Arg.Set sure         , "Keeps only the sure solutions");
  ("-minimize"     , Arg.Set minimizing   , "Specify that the problem is a minimization problem");
  (*********************************************** ALIASES ********************************************************)
  ("-m"            , Arg.Set minimizing   , "Alias for -minimize");
  ("-t"            , Arg.Set trace        , "Alias for -trace");
  ("-s"            , Arg.Set sure         , "Alias for -sure");
  ("-v"            , Arg.Set visualization, "Alias for -visualization");
  ("-p"            , Arg.Float set_prec   , "Alias for -precision");
  ("-d"            , Arg.String set_domain, "Alias for -domain");
]

let anonymous_arg = Constant.set_prob

let parse_args () = Arg.parse speclist anonymous_arg ""

let _ =
  let open Constant in
  parse_args ();
  let prob = File_parser.parse !problem in
  if !trace then Format.printf "%a" Csp.print prob;
  if !minimizing then
    match !domain with
    | "box" -> Minimizer.Box.minimizing prob
    | "boxCP" -> Minimizer.BoxCP.minimizing prob
    | "oct" -> Minimizer.Oct.minimizing prob
    | "poly" -> Minimizer.Poly.minimizing prob
    | "boxNoct" -> Minimizer.BoxNOct.minimizing_various prob
    | "boxNpoly" -> Minimizer.BoxNPoly.minimizing_various prob
    | "octNpoly" -> Minimizer.OctNPoly.minimizing_various prob
    | _ -> "domain undefined "^(!domain) |> failwith
  else
    match !domain with
    | "box" -> SBox.go prob
    (* | "boxCP" -> SBoxCP.go prob *)
    | "oct" -> SOctCP.go prob
    | "poly" -> SPolyCP.go prob
    (* | "boxNoct" -> SBoxNOct.go prob *)
    (* | "boxNpoly" -> SBoxNPoly.go prob *)
    (* | "octNpoly" -> SOctNPoly.go prob *)
    | _ -> "domain undefined "^(!domain) |> failwith
