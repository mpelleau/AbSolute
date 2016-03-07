open Apron
open ADCP

let solving = ref true 

let get_solving_problem p =
  match p with
  | "gear4" -> Problems.gear4
  | "st_miqp5" -> Problems.st_miqp5
  | "nonlin1" -> Problems.nonlin1
  | "nonlin2" -> Problems.nonlin2
  | "two_circles" -> Problems.two_circles
  | "one_circle" -> Problems.one_circle
  | _ -> "solving problem undefined "^p |> failwith

let get_minimization_problem p =
  match p with
  | "test" -> Problems.test
  | _ -> "minimization problem undefined "^p |> failwith

let parse_args () =
  let rec doit args = match args with
  | "-precision"::x::r -> Constant.precision := float_of_string x; doit r
  | "-max_iter"::x::r -> Constant.max_iter := int_of_string x; doit r
  | "-domain_s"::x::r -> Constant.domain_solving := x; doit r
  | "-domain_m"::x::r -> Constant.domain_minimizing:= x; doit r
  | "-visualization"::r ->Constant.visualization:=true; doit r
  | x::r -> Constant.problem:=x; doit r
  | [] -> ()
  in
  Array.to_list Sys.argv |> List.tl |> doit

let main =
  let open Constant in
  parse_args ();
  solving := !Constant.problem <> "test";
  if !Constant.visualization then Vue.create_window 800 800;
  if !solving then
    match !domain_solving with
    | "box" -> Solver.Box.solving (get_solving_problem !problem)
    | "oct" -> Solver.Oct.solving (get_solving_problem !problem)
    | "poly" -> Solver.Poly.solving (get_solving_problem !problem)
    | _ -> "domain undefined"^(!domain_solving) |> failwith
  else
    match !domain_minimizing with
    | "octbox" -> Minimizer.OctBox.minimizing (get_minimization_problem !problem)
    | "octminmax" -> Minimizer.OctMinMax.minimizing (get_minimization_problem !problem)
    | "octminmin" -> Minimizer.OctMinMin.minimizing (get_minimization_problem !problem)
    | _ -> "domain undefined"^(!domain_minimizing) |> failwith
