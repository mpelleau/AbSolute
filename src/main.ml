open Apron
open ADCP
open Itv
open Bot

let solving = ref true 

let speclist = [
  ("-visualization", Arg.Set Constant.visualization     , "Enables visualization mode");
  ("-precision"    , Arg.Float ((:=) Constant.precision), "Changes the precision. default is 1e-3");
  ("-max_sol"      , Arg.Int ((:=) Constant.max_sol)    , "Changes the maximum number of solutions. default is 1e6");
  ("-max_iter"     , Arg.Int ((:=) Constant.max_iter)   , "Changes the maximum number of iterations. default is 1e7");
  ("-domain"       , Arg.String ((:=) Constant.domain)  , "Changes the domain used for the solving. default is box");
  ("-obj"          , Arg.Set Constant.obj               , "Generates an .obj file (for 3D visualization)");
  ("-tex"          , Arg.Set Constant.tex               , "Prints the solutions in latex format on stadard output");
  ("-pruning"      , Arg.Set Constant.pruning           , "Enables the \"pruning\" during the solving process");
  ("-trace"        , Arg.Set Constant.trace             , "Prints the solutions on standard output");
  ("-minimize"     , Arg.Set Constant.minimizing        , "Specify that the problem is a minimization problem");
  (*********************************************** ALIASES ********************************************************)
  ("-m"            , Arg.Set Constant.minimizing        , "Alias for -minimize");
  ("-t"            , Arg.Set Constant.trace             , "Alias for -trace");
  ("-v"            , Arg.Set Constant.visualization     , "Alias for -visualization");
  ("-p"            , Arg.Float ((:=) Constant.precision), "Alias for -precision");
  ("-d"            , Arg.String ((:=) Constant.domain)  , "Alias for -domain");
]

let anonymous_arg s = 
  if Sys.file_exists s then Constant.problem := s
  else failwith (Format.sprintf "%s must be a filename" s)

let parse_args () = Arg.parse speclist anonymous_arg ""

let main =
  let open Constant in
  parse_args ();
  let prob = File_parser.parse !problem in
  (* Syntax.print Format.std_formatter prob; *)
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
    | "box" -> Solver.Box.solving prob
    | "boxCP" -> Solver.BoxCP.solving prob
    | "oct" -> Solver.Oct.solving prob
    | "poly" -> Solver.Poly.solving prob 
    | "boxNoct" -> Solver.BoxNOct.solving_various prob
    | "boxNpoly" -> Solver.BoxNPoly.solving_various prob
    | "octNpoly" -> Solver.OctNPoly.solving_various prob
    | _ -> "domain undefined "^(!domain) |> failwith
