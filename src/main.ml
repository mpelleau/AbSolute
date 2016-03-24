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
  | "-precision"::x::r -> Constant.precision := float_of_string x; doit r
  | "-max_sol"::x::r -> Constant.max_sol := int_of_string x; doit r
  | "-max_iter"::x::r -> Constant.max_iter := int_of_string x; doit r
  | "-domain_s"::x::r -> Constant.domain_solving := x; doit r
  | "-domain_m"::x::r -> Constant.domain_minimizing:= x; doit r
  | "-visualization"::r ->Constant.visualization:=true; doit r
  | x::r -> Constant.problem:=x; doit r
  | [] -> ()
  in
  Array.to_list Sys.argv |> List.tl |> doit

let main =
  (* let itv = ItvF.of_bounds (Bound_float.of_float_down 9.424778) (Bound_float.of_float_up 15.707963) in *)
  (* let itv' = ItvF.sin itv in *)
  (* let itv'' = ItvF.filter_sin itv (ItvF.of_ints 0 1) in *)
  (* let itv''' = ItvF.filter_sin itv (ItvF.of_ints (-1) 0) in *)
  (* Format.printf "%s ; %s ; " (ItvF.to_string itv) (ItvF.to_string itv'); *)
  (* match itv'' with *)
  (* | Bot -> Format.printf "_|_ ; "; *)
  (* | Nb x -> Format.printf "%s ; " (Bot.bot_to_string ItvF.to_string x); *)
  (* match itv''' with *)
  (* | Bot -> Format.printf "_|_\n"; *)
  (* | Nb x -> Format.printf "%s\n" (Bot.bot_to_string ItvF.to_string x); *)

  (* let itv = ItvF.of_bounds (Bound_float.of_float_down 6.283186) (Bound_float.of_float_up 15.707963) in *)
  (* let itv' = ItvF.sin itv in *)
  (* let itv'' = ItvF.filter_sin itv (ItvF.of_ints 0 1) in *)
  (* let itv''' = ItvF.filter_sin itv (ItvF.of_ints (-1) 0) in *)
  (* Format.printf "%s ; %s ; " (ItvF.to_string itv) (ItvF.to_string itv'); *)
  (* match itv'' with *)
  (* | Bot -> Format.printf "_|_ ; "; *)
  (* | Nb x -> Format.printf "%s ; " (Bot.bot_to_string ItvF.to_string x); *)
  (* match itv''' with *)
  (* | Bot -> Format.printf "_|_\n"; *)
  (* | Nb x -> Format.printf "%s\n" (Bot.bot_to_string ItvF.to_string x); *)

  (* let itv = ItvF.of_bounds (Bound_float.of_float_down 10.424778) (Bound_float.of_float_up 12.707964) in *)
  (* let itv' = ItvF.sin itv in *)
  (* let itv'' = ItvF.filter_sin itv (ItvF.of_ints 0 1) in *)
  (* let itv''' = ItvF.filter_sin itv (ItvF.of_ints (-1) 0) in *)
  (* Format.printf "%s ; %s ; " (ItvF.to_string itv) (ItvF.to_string itv'); *)
  (* match itv'' with *)
  (* | Bot -> Format.printf "_|_ ; "; *)
  (* | Nb x -> Format.printf "%s ; " (Bot.bot_to_string ItvF.to_string x); *)
  (* match itv''' with *)
  (* | Bot -> Format.printf "_|_\n"; *)
  (* | Nb x -> Format.printf "%s\n" (Bot.bot_to_string ItvF.to_string x); *)


  let open Constant in
  parse_args ();
  solving := !Constant.problem <> "test";
  if !Constant.visualization then Vue.create_window 800 800;
  let prob = File_parser.parse !problem in
  Syntax.print Format.std_formatter prob;
  if !solving then
    match !domain_solving with
    | "box" -> Solver.Box.solving prob
    | "oct" -> Solver.Oct.solving prob
    | "poly" -> Solver.Poly.solving prob 
    (*| "boxNoct" -> VariousDA.BoxNOct.solving prob
    | "boxNpoly" -> VariousDA.BoxNPoly.solving prob
    | "octNpoly" -> VariousDA.OctNPoly.solving prob*)
    | _ -> "domain undefined"^(!domain_solving) |> failwith
  else(*
    match !domain_minimizing with
    | "octbox" -> Minimizer.OctBox.minimizing (get_minimization_problem !problem)
    | "octminmax" -> Minimizer.OctMinMax.minimizing (get_minimization_problem !problem)
    | "octminmin" -> Minimizer.OctMinMin.minimizing (get_minimization_problem !problem)
    | _ -> "domain undefined"^(!domain_minimizing) |> failwith
      *)
    failwith "minimization not implemented yet"
