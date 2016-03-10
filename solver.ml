open Apron
open Mpqf
open Format
open Utils
open ADCP

module Solve(Abs : AbstractCP) =
  struct
    let man = Abs.get_manager

    type consistency = Empty | Full | Maybe

    let consistency abs tab =
      if Abstract1.is_bottom man abs then Empty,abs
      else 
	let abs' = Abstract1.meet_tcons_array man abs tab in
	(if tcons_for_all (Abstract1.sat_tcons man abs') tab then Full
	else if Abstract1.is_bottom man abs then Empty else Maybe),abs'
	  
    let draw abs info col =
      if !Constant.visualization then 
	Vue.draw (Abs.points_to_draw abs) col info

    let explore abs env tab nb_steps nb_sol =
      let info = Vue.get_info (Abs.points_to_draw abs) in
      draw abs info Graphics.yellow;
      let rec aux abs env nb_steps nb_sol =
	let cons,abs' = consistency abs tab in
	match cons with
	| Empty -> (nb_steps, nb_sol)
	| Full -> draw abs' info Graphics.blue;(nb_steps, nb_sol+1)
	| Maybe  ->
	  (match (Abs.is_small abs' !Constant.precision) with
	  | true,_ -> 
	    draw abs' info Graphics.green;
	    (nb_steps, nb_sol+1)
	  | _,exprs -> 
	    draw abs' info Graphics.yellow;
            Abs.split abs' exprs |>
            List.fold_left (fun (a, b) c -> aux c env (a+1) b) (nb_steps, nb_sol))
      in aux abs env nb_steps nb_sol

    let solving env domains cons =
      let abs = Abs.of_lincons_array env domains in
      printf "abs = %a@." Abstract1.print abs;
      let box = Abstract1.to_box man abs in
      let tab = box.Abstract1.interval_array in
      printf "box = %a@." (print_array Interval.print) tab;
      let s = Manager.get_funopt man Manager.Funid_meet_tcons_array in
      let s' = {s with Manager.algorithm = 100} in
      Manager.set_funopt man Manager.Funid_meet_tcons_array s';
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
module Oct = Solve(OctBoxCP)
module Poly = Solve(PolyCP)
