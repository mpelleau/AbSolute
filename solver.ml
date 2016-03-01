open Apron
open Mpqf
open Format
open Utils
open ADCP

let print_sol box =
  let open Interval in
  let itv = box.Abstract1.interval_array in
  printf "[| ";
  Array.iter (fun e -> printf "[%f; %f];" (scalar_to_float e.inf) (scalar_to_float e.sup)) itv;
  printf "|] "

module Solve(Abs : AbstractCP) =
  struct
    let man = Abs.get_manager
    let consistency abs tab max_iter =
      let rec cons_loop abs n =
        if n >= max_iter || Abstract1.is_bottom man abs then abs
        else(
          let abs_tmp = Abstract1.copy man abs in
          Abstract1.meet_tcons_array_with man abs tab;		
          if Abstract1.is_eq man abs_tmp abs then abs
          else cons_loop abs (n+1)
        )
      in
      let abs' = cons_loop abs 0 in
      abs'
    let rec explore abs env tab max_iter prec nb_steps nb_sol =
      let abs' = consistency abs tab max_iter in
      if Abstract1.is_bottom man abs' then
        (* No solutions in this sub-tree. *)
        (nb_steps, nb_sol)
      else
        (* Keep on searching in this sub-tree. *)
        let (small, exprs) = Abs.is_small abs' prec in
        if small then
          (* Solution found! *)
          (
          let box = Abstract1.to_box man abs' in
          print_sol box;
          (nb_steps, nb_sol+1)
          )
        else
          (* Split the next variable, heuristic *)
          let list_abs = Abs.split abs' exprs in
          List.fold_left (fun (nbe, nbsol) absi -> 
	    explore absi env tab max_iter prec (nbe+1) nbsol) (nb_steps, nb_sol
	  ) list_abs

    let solving env domains cons =
      let max_iter = !Constant.max_iter and prec = !Constant.precision in
      let abs = Abs.of_lincons_array env domains in
      printf "abs = %a@." Abstract1.print abs;
      let box = Abstract1.to_box man abs in
      let tab = box.Abstract1.interval_array in
      printf "box = %a@." (print_array Interval.print) tab;
      let s = Manager.get_funopt man Manager.Funid_meet_tcons_array in
      let s' = {s with Manager.algorithm = 100;} in
      Manager.set_funopt man Manager.Funid_meet_tcons_array s';(**)
      if not (Abstract1.is_bottom man abs) then
        let (nb_steps, nb_sol) = explore abs env cons max_iter prec 1 0 in
	match nb_sol with
	| 0 -> printf "No solutions - #created nodes: %d@." nb_steps
	| 1 -> printf "Unique solution - #created nodes: %d@." nb_steps
        | _ -> printf "#solutions: %d - #created nodes: %d@." nb_sol nb_steps
      else
        printf "No Solutions - #created nodes: 0@."
  end






let main =
  (*let x = Var.of_string "x" in
  let y = Var.of_string "y" in
  let env = Environment.make [|x|] [|y|] in
  let domains = Parser.lincons1_of_lstring env ["x>=-5"; "x<=5"; "y>=-5"; "y<=5"] in
  (*let tab = ["x^2+y^2=4"; "(x-2)^2+(y+1)^2=1"] in*)
  let tab = ["y-x^2=0"; "y+x^2=2"] in
  let constraints = Parser.tcons1_of_lstring env tab in*)

  let (env, domains, andor, constraints) = Problems.gear4 in

  let module SolverBox = Solve(BoxCP) in
  SolverBox.solving env domains constraints;

  let module SolverOct = Solve(OctMinMinCP) in
  SolverOct.solving env domains constraints;

  let module SolverPoly = Solve(PolyCP) in
  SolverPoly.solving env domains constraints;
