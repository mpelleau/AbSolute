open Apron
open Mpqf
open Format
open Utils
open ADCP

module Solve(Abs : AbstractCP) =
  struct
    let man = Abs.get_manager
    let consistency abs tab =
      let rec cons_loop abs n =
        if n >= !Constant.max_iter || Abstract1.is_bottom man abs then abs
        else(
          let abs_tmp = Abstract1.copy man abs in
          Abstract1.meet_tcons_array_with man abs tab;		
          if Abstract1.is_eq man abs_tmp abs then abs
          else cons_loop abs (n+1)
        )
      in cons_loop abs 0

    let explore abs env tab nb_steps nb_sol =
      let info = Vue.get_info (Abs.points_to_draw abs) in
      let draw abs col =
	if !Constant.visualization then 
	  Vue.draw (Abs.points_to_draw abs) col info
      in
      draw abs Graphics.red;
      let rec aux abs env nb_steps nb_sol =
	draw abs Graphics.yellow;
	let abs' = consistency abs tab in
	draw abs' Graphics.yellow;
	if Abstract1.is_bottom man abs' then(
          (* No solutions in this sub-tree. *)
          (nb_steps, nb_sol)
	)
	else
          let (small, exprs) = Abs.is_small abs' !Constant.precision in
	  if small then
            (* Keep on searching in this sub-tree. *)
            (* Solution found! *)
            (
              (* let box = Abstract1.to_box man abs' in
		 print_sol box; *)
	      draw abs' Graphics.green;
              (nb_steps, nb_sol+1)
            )
          else begin
            (* Split the next variable, heuristic *)
            let list_abs = Abs.split abs' exprs in
            List.fold_left (fun (nbe, nbsol) absi ->
	      aux absi env (nbe+1) nbsol) (nb_steps, nb_sol
	    ) list_abs
	  end
      in 
      let return = aux abs env nb_steps nb_sol in
      return

    let solving env domains cons =
      let abs = Abs.of_lincons_array env domains in
      printf "abs = %a@." Abstract1.print abs;
      let box = Abstract1.to_box man abs in
      let tab = box.Abstract1.interval_array in
      printf "box = %a@." (print_array Interval.print) tab;
      let s = Manager.get_funopt man Manager.Funid_meet_tcons_array in
      let s' = {s with Manager.algorithm = 100;} in
      Manager.set_funopt man Manager.Funid_meet_tcons_array s';(**)
      if not (Abstract1.is_bottom man abs) then
        let (nb_steps, nb_sol) = explore abs env cons 1 0 in
	match nb_sol with
	| 0 -> printf "No solutions - #created nodes: %d@." nb_steps
	| 1 -> printf "Unique solution - #created nodes: %d@." nb_steps
        | _ -> printf "#solutions: %d - #created nodes: %d@." nb_sol nb_steps
      else
        printf "No Solutions - #created nodes: 0@."

    let solving solving_problem =
      let (env, domains, _, constraints) = solving_problem in
      solving env domains constraints
  end

module Box = Solve(BoxCP)
module Oct = Solve(OctMinMinCP)
module Poly = Solve(PolyCP)
