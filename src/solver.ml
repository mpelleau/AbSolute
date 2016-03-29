open Apron
open Mpqf
open Format
open Utils
open ADCP

module Solve(Abs : AbstractCP) =
  struct

    let consistency abs tab =
      let abs' = List.fold_left Abs.meet abs tab in
      (if Abs.is_bottom abs' then `Empty 
       else if List.for_all (Abs.sat_cons abs') tab then `Full
       else `Maybe)
       ,abs'
	
    let draw abs info col vars =
      if !Constant.visualization then
	Vue.draw (Abs.points_to_draw abs vars) col info

    let explore abs constrs nb_steps nb_sol vars =
      let info = Vue.get_info (Abs.points_to_draw abs vars) in
      draw abs info Graphics.yellow vars;
      let rec aux abs nb_steps nb_sol =
	let cons,abs' = consistency abs constrs in	
	match cons with
	| `Empty -> (nb_steps, nb_sol)
	| `Full ->  draw abs' info Graphics.blue vars; (nb_steps, nb_sol+1)
	| `Maybe  ->
	  (match (Abs.is_small abs' !Constant.precision) with
	  | true,_ -> draw abs' info Graphics.green vars; (nb_steps, nb_sol+1)
	  | _,exprs when nb_sol <= !Constant.max_sol ->
	    draw abs' info Graphics.yellow vars;
            Abs.split abs' exprs |>
            List.fold_left (fun (a, b) c -> aux c (a+1) b) (nb_steps, nb_sol)
	  | _ -> (nb_steps, nb_sol)
	  )
      in aux abs nb_steps nb_sol

    let explore_breath_first abs env tab nb_steps nb_sol vars=
      let info = Vue.get_info (Abs.points_to_draw abs vars) in
      let nb_steps = ref nb_steps and nb_sol = ref nb_sol in
      let queue = Queue.create () in
      draw abs info Graphics.yellow vars;
      Queue.add abs queue;
      while Queue.is_empty queue |> not do
	let cons,abs' = consistency (Queue.take queue) tab in
	match cons with
	| `Empty -> ()
	| `Full -> draw abs' info Graphics.blue vars; incr nb_sol
	| `Maybe  ->
	  (match (Abs.is_small abs' !Constant.precision) with
	  | true,_ -> draw abs' info Graphics.green vars; incr nb_sol
	  | _,exprs when !nb_sol < !Constant.max_sol ->
	    draw abs' info Graphics.yellow vars;
            Abs.split abs' exprs |> List.iter (fun e -> incr nb_steps; Queue.add e queue)
	  | _ -> draw abs' info Graphics.green vars
	  )
      done;
      !nb_steps,!nb_sol

    let solving prob =
      let open Syntax in
      let abs = Abs.of_problem prob in
      printf "abs = %a@." Abs.print abs;
      if not (Abs.is_bottom abs) then
        let (nb_steps, nb_sol) = explore abs prob.constraints 1 0 prob.to_draw in
	Format.printf "solving ends\n%!";
	match nb_sol with
	| 0 -> printf "No solutions - #created nodes: %d@." nb_steps
	| 1 -> printf "Unique solution - #created nodes: %d@." nb_steps
        | _ -> printf "#solutions: %d - #created nodes: %d@." nb_sol nb_steps
      else
        printf "No Solutions - #created nodes: 0@."

    let solving_various prob =
      let open Syntax in
      let abs = Abs.of_problem prob in
      printf "abs = %a" Abs.print abs;
      if not (Abs.is_bottom abs) then
        let cons = List.filter (fun exp -> not (is_cons_linear exp)) prob.constraints in
        Format.printf "\ncons = [";
        List.iter (Format.printf "%a ;" (print_bexpr)) cons;
        Format.printf "]\n";
        let (nb_steps, nb_sol) = explore abs cons 1 0 prob.to_draw in
	Format.printf "solving ends\n%!";
	match nb_sol with
	| 0 -> printf "No solutions - #created nodes: %d@." nb_steps
	| 1 -> printf "Unique solution - #created nodes: %d@." nb_steps
        | _ -> printf "#solutions: %d - #created nodes: %d@." nb_sol nb_steps
      else
        printf "No Solutions - #created nodes: 0@."

  end

module Box = Solve(Abstract_box.BoxF)
module BoxCP = Solve(BoxCP)
module Oct = Solve(OctBoxCP)
module Poly = Solve(PolyCP)

module BoxNOct = Solve(VariousDA.BoxNOct)
module BoxNPoly = Solve(VariousDA.BoxNPoly)
module OctNPoly = Solve(VariousDA.OctNPoly)
