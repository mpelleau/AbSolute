open Apron
open ADCP
open Itv
open Bot

let solving = ref true 

let get_variousDA_problem p =
  match p with
  | "test" -> Problems.test
  | _ -> "minimization problem undefined "^p |> failwith

let parse_args () =
  let rec doit args = match args with
  | "-precision"::x::r 
  | "-p"::x::r -> Constant.precision := float_of_string x; doit r
  | "-max_sol"::x::r -> Constant.max_sol := int_of_string x; doit r
  | "-max_iter"::x::r -> Constant.max_iter := int_of_string x; doit r
  | "-domain_s"::x::r -> Constant.domain_solving := x; doit r
  | "-domain_m"::x::r -> Constant.domain_minimizing:= x; doit r
  | "-visualization"::r 
  | "-v"::r ->Constant.visualization:=true; doit r
  | x::r -> Constant.problem:=x; doit r
  | [] -> ()
  in Array.to_list Sys.argv |> List.tl |> doit

let main =
  (* let itv = ItvF.of_bounds (Bound_float.of_float_down (-2.5)) (Bound_float.of_float_up 5.0) in *)
  (* let itv = ItvF.of_bounds (Bound_float.of_float_down 2.5) (Bound_float.of_float_up 5.0) in *)
  (* let itv = ItvF.of_bounds (Bound_float.of_float_down (-5.0)) (Bound_float.of_float_up (-2.5)) in *)
  (* let itv' = ItvF.pow itv (ItvF.of_int 2) in *)
  (* let itv2 = ItvF.pow itv (ItvF.of_int 3) in *)
  (* let itv'' = ItvF.filter_pow itv (ItvF.of_floats 0. infinity) (ItvF.of_int 2) in *)
  (* let itv''' = ItvF.filter_pow itv (ItvF.of_floats neg_infinity 0.) (ItvF.of_int 2) in *)
  (* let itv'''' = ItvF.filter_pow itv (ItvF.of_ints 0 0) (ItvF.of_int 2) in *)
  (* Format.printf "%s ; %s (%s, %s) ; %s (%s, %s) ; " (ItvF.to_string itv) (ItvF.to_string itv') (Bound_float.to_string (fst itv')) (Bound_float.to_string (snd itv')) (ItvF.to_string itv2) (Bound_float.to_string (fst itv2)) (Bound_float.to_string (snd itv2)); *)
  (* match itv'' with *)
  (* | Bot -> Format.printf "_|_ ; "; *)
  (* | Nb x -> Format.printf "%s ; " (ItvF.to_string x); *)
  (* match itv''' with *)
  (* | Bot -> Format.printf "_|_ ; "; *)
  (* | Nb x -> Format.printf "%s ; " (ItvF.to_string x); *)
  (* match itv'''' with *)
  (* | Bot -> Format.printf "_|_\n"; *)
  (* | Nb x -> Format.printf "%s\n" (ItvF.to_string x); *)


  let open Constant in
  parse_args ();
  solving := !Constant.problem <> "test";
  if !Constant.visualization then Vue.create_window 800 800;
  let prob = File_parser.parse !problem in
  (* Syntax.print Format.std_formatter prob; *)
  if !solving then
    match !domain_solving with
    | "box" -> Solver.Box.solving prob
    | "boxCP" -> Solver.BoxCP.solving prob
    | "oct" -> Solver.Oct.solving prob
    | "poly" -> Solver.Poly.solving prob 
    | "boxNoct" -> Solver.BoxNOct.solving prob
    | "boxNpoly" -> Solver.BoxNPoly.solving prob
    | "octNpoly" -> Solver.OctNPoly.solving prob
    | _ -> "domain undefined"^(!domain_solving) |> failwith
  else(*
    match !domain_minimizing with
    | "octbox" -> Minimizer.OctBox.minimizing (get_minimization_problem !problem)
    | "octminmax" -> Minimizer.OctMinMax.minimizing (get_minimization_problem !problem)
    | "octminmin" -> Minimizer.OctMinMin.minimizing (get_minimization_problem !problem)
    | _ -> "domain undefined"^(!domain_minimizing) |> failwith
      *)
    failwith "minimization not implemented yet"
