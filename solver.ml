open Apron
open Mpqf
open Format
open Utils
open ADCP

module Solve(Abs : AbstractCP) =
  struct
    let man = Abs.get_manager

    type consistency = Bottom | Sure | Maybe

    let consistency abs tab =
      if Abstract1.is_bottom man abs then abs
      else Abstract1.meet_tcons_array man abs tab

    let draw abs info col =
      if !Constant.visualization then 
	Vue.draw (Abs.points_to_draw abs) col info

    let explore abs env tab nb_steps nb_sol =
      let info = Vue.get_info (Abs.points_to_draw abs) in
      draw abs info Graphics.yellow;
      let rec aux abs env nb_steps nb_sol =
	let abs' = consistency abs tab in
	if Abstract1.is_bottom man abs' then(
	  draw abs info Graphics.red;	  
          (* No solutions in this sub-tree. *)
          (nb_steps, nb_sol)
	)
	else(
	  if tcons_for_all (Abstract1.sat_tcons man abs') tab then begin
	    draw abs' info Graphics.blue;
	    (nb_steps, nb_sol+1)
	  end  
	  else begin
	    Format.printf "sat_cons false\n";
            let (small, exprs) = Abs.is_small abs' !Constant.precision in
	    if small then(
	      draw abs' info Graphics.green;
	      (nb_steps, nb_sol+1)
            )else begin
	      Format.printf "small false\n";      
	      draw abs' info Graphics.yellow;
              (* Split the next variable, heuristic *)
              let list_abs = Abs.split abs' exprs in
              List.fold_left (fun (nbe, nbsol) absi ->
		aux absi env (nbe+1) nbsol) (nb_steps, nb_sol
	      ) list_abs
	    end
	  end)
      in 
      let return = aux abs env nb_steps nb_sol in
      return
	
    (* Same as explore but with Breadth-first search *)
    let explore2 abs env tab nb_steps nb_sol =
      let info = Vue.get_info (Abs.points_to_draw abs) in
      let queue = Queue.create() in
      Queue.add abs queue;
      let nb_step = ref 0
      and nb_sol = ref 0 in
      while not (Queue.is_empty queue) do
	let abs = Queue.take queue in
	draw abs info Graphics.yellow;
	let abs' = consistency abs tab in
	draw abs' info Graphics.yellow;
	if Abstract1.is_bottom man abs' then ()
	else if tcons_for_all (Abstract1.sat_tcons man abs') tab then begin
	  draw abs' info Graphics.blue;
	  incr nb_sol
	end  
	else
          let (small, exprs) = Abs.is_small abs' !Constant.precision in
	  if small then(
	    draw abs' info Graphics.green;
	    incr nb_sol
          )
          else begin
            (* Split the next variable, heuristic *)
            let list_abs = Abs.split abs' exprs in
	    List.iter (fun e -> Queue.add e queue; incr nb_step) list_abs;
	  end
      done;
      (!nb_step),(!nb_sol)

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
