open Apron
open Mpqf
open Format
open Utils
open ADCP

(* Splitting strategy handler *)
module Splitter (Abs : AbstractCP) = struct
  type consistency = Full of Abs.t 
		   | Maybe of Abs.t * Syntax.bexpr list
		   | Empty
			 
  let consistency abs constrs : consistency =
    try
      let abs' = List.fold_left Abs.meet abs constrs in
      let unsat = List.filter (fun c -> not (Abs.sat_cons abs' c)) constrs in
      match unsat with
      | [] -> Full abs'
      | _ -> if Abs.is_bottom abs' then Empty else Maybe(abs', unsat)
    with Bot.Bot_found -> Empty
	
  let split abs expr = Abs.split abs expr
end

(* Solver *)
module Solve(Abs : AbstractCP) = struct

  include Splitter(Abs)

  let draw abs info col vars =
    if !Constant.visualization then
      let points = Abs.points_to_draw abs vars in
      if List.length points > 0 then Vue.draw points col info

  let print_sol_for_latex abs vars =
    if !Constant.tex then
      let points = Abs.points_to_draw abs vars in
      if points <> [] then (
	Format.printf "  \\filldraw[rose, fill opacity = 0.3] ";
	List.iter (fun (x,y) -> Format.printf "(%f, %f) -- " x y) (Vue.graham_sort points);
	Format.printf "cycle;@.";
      )
	
  let explore abs constrs vars =
    let info = Vue.get_info (Abs.points_to_draw abs vars) in
    draw abs info Graphics.yellow vars;
    let rec aux abs cstrs nb_steps nb_sol =
      match consistency abs cstrs with
      | Empty -> (nb_steps, nb_sol)
      | Full(abs') -> print_sol_for_latex abs' vars; draw abs' info (Graphics.rgb 0 191 255) vars; (nb_steps, nb_sol+1)
      | Maybe(abs',cstrs)  ->
	(match (Abs.is_small abs' !Constant.precision) with
	| true,_ -> print_sol_for_latex abs' vars; draw abs' info Graphics.green vars; (nb_steps, nb_sol+1)
	| _,exprs when nb_sol <= !Constant.max_sol ->
	  draw abs' info Graphics.yellow vars;
          Abs.split abs' exprs |>
	      List.fold_left (fun (a, b) c -> aux c cstrs (a+1) b) (nb_steps, nb_sol)
	| _ -> (nb_steps, nb_sol)
	)
    in 
    let res = aux abs constrs 1 0 in 
    if !Constant.visualization then Vue.draw_end info;
    res
      
  let explore_breath_first abs constrs vars =
    let info = Vue.get_info (Abs.points_to_draw abs vars) in
    let nb_steps = ref 1 and nb_sol = ref 0 in
    let queue = Queue.create () in
    draw abs info Graphics.yellow vars;
    Queue.add (abs,constrs) queue;
    while Queue.is_empty queue |> not do
      let a,c = Queue.take queue in
      match consistency a c with
      | Empty -> ()
      | Full(abs') -> draw abs' info (Graphics.rgb 0 191 255) vars; incr nb_sol
      | Maybe(abs',cons)  ->
	(match (Abs.is_small abs' !Constant.precision) with
	| true,_ -> draw abs' info Graphics.green vars; incr nb_sol
	| _,exprs when !nb_sol < !Constant.max_sol ->
	  draw abs' info Graphics.yellow vars;
          Abs.split abs' exprs |> List.iter (fun e -> incr nb_steps; Queue.add (e,cons) queue)
	| _ -> draw abs' info Graphics.green vars
	)
    done;
    if !Constant.visualization then Vue.draw_end info;
    !nb_steps,!nb_sol
      
  let solving prob =
    let open Syntax in
    let abs = Abs.of_problem prob in
    printf "abs = %a@." Abs.print abs;
    if not (Abs.is_bottom abs) then
      let (nb_steps, nb_sol) = explore abs prob.constraints prob.to_draw in
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
      Format.printf "\nconstraints = [";
      List.iter (Format.printf "%a ;" (print_bexpr)) prob.constraints;
      Format.printf "]@.";
      Format.printf "non linear constraints = [";
      List.iter (Format.printf "%a ;" (print_bexpr)) cons;
      Format.printf "]@.";
      let (nb_steps, nb_sol) = explore abs cons prob.to_draw in
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
