open Apron
open ADCP

let solving = ref true 

let get_solving_problem p =
  solving := true;
  match p with
  | "gear4" -> Problems.gear4
  | "st_miqp5" -> Problems.st_miqp5
  | _ -> "problem undefined"^p |> failwith

let get_minimization_problem p =
  solving := false;
  match p with
  | "test" -> Problems.test
  | _ ->  "problem undefined"^p |> failwith

let parse_args () =
  let rec doit args = match args with
  | "-precision"::x::r -> Constant.precision := float_of_string x; doit r
  | "-max_iter"::x::r -> Constant.max_iter := int_of_string x; doit r
  | "-domain"::x::r -> Constant.domain := x; doit r
  | x::r -> Constant.problem:=x; doit r
  | [] -> ()
  in
  Array.to_list Sys.argv |> List.tl |> doit


let main =
  let open Constant in
  parse_args ();
  if !solving then begin
    match !domain with
    | "box" -> Solver.SolverBox.solving (get_solving_problem !problem)
    | "oct" -> Solver.SolverOct.solving (get_solving_problem !problem)
    | "poly" -> Solver.SolverPoly.solving (get_solving_problem !problem)
    | _ -> "domain undefined"^(!domain) |> failwith
  end
