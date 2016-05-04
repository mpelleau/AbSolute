open Apron
open ADCP
open Itv
open Bot

let solving = ref true 

let parse_args () =
  let rec doit args = match args with
  | "-precision"::x::r 
  | "-p"::x::r -> Constant.precision := float_of_string x; doit r
  | "-max_sol"::x::r -> Constant.max_sol := int_of_string x; doit r
  | "-max_iter"::x::r -> Constant.max_iter := int_of_string x; doit r
  | "-domain"::x::r -> Constant.domain := x; doit r
  | "-minimize"::r 
  | "-m"::r -> Constant.minimizing:= true; doit r
  | "-visualization"::r 
  | "-v"::r ->Constant.visualization:=true; doit r
  | "-obj"::r ->Constant.obj:=true; doit r
  | "-tex"::r ->Constant.tex:=true; doit r
  | x::r -> Constant.problem:=x; doit r
  | [] -> ()
  in Array.to_list Sys.argv |> List.tl |> doit

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
