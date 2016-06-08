open Format
open Utils
open ADCP
open Adcp_sig

module Minimize(Abs : AbstractCP) = struct

  include Splitter.Make(Abs)
  module Printer = Out.Make(Abs)

  let draw abs info col vars =
    if !Constant.visualization then
      Vue.draw (Abs.points_to_draw abs vars) col info

  let explore abs constrs obj vars =
    let info = Vue.get_info (Abs.points_to_draw abs vars) in
    draw abs info Graphics.yellow vars;
    let rec aux abs best_value nb_steps sols =
      match consistency abs constrs with
      | Empty -> (nb_steps, best_value, sols)
      | Full a | Maybe (a,_)->
        let (obj_value, _) = Abs.forward_eval a obj in
        if obj_value > best_value then
          (* There's no point in keep on searching in this branch *)
	  (draw a info Graphics.yellow vars;
           (nb_steps, best_value, sols))
        else
          (match (Abs.is_small a) with
	  | true ->
            if obj_value < best_value then
	      (List.iter (fun a -> draw a info Graphics.yellow vars) sols;
	       draw a info (Graphics.rgb 0 191 255) vars;
               (nb_steps, obj_value, [a]))
            else
	      (draw a info (Graphics.rgb 0 191 255) vars;
               (nb_steps, obj_value, a::sols))
	  | _ when (List.length sols) <= !Constant.max_sol ->
            Abs.split a |>
		List.fold_left (fun (a, b, c) d -> aux d b (a+1) c) (nb_steps, best_value, sols)
          | _ -> (nb_steps, best_value, sols)
	  )
    in 
    let (_, obj_sup) = Abs.forward_eval abs obj in
    let res = aux abs obj_sup 1 [] in
    if !Constant.visualization then Vue.draw_end info;
    res
      
  let minimizing prob =
    let open Syntax in
      let abs = init prob in
      printf "abs = %a@." Abs.print abs;
      if not (Abs.is_bottom abs) then
        let (nb_steps, best_value, sols) = explore abs prob.constraints prob.objective prob.to_draw in
	Format.printf "solving ends\n%!";
        let nb_sol = List.length sols in
	match nb_sol with
	| 0 -> printf "No solutions - #created nodes: %d@." nb_steps
	| 1 -> printf "Unique solution - #created nodes: %d - best_value = %f@." nb_steps best_value
        | _ -> printf "#solutions: %d - #created nodes: %d - best_value = %f@." nb_sol nb_steps best_value
      else
        printf "No Solutions - #created nodes: 0@."
	  
    let minimizing_various prob =
      let open Syntax in
      let abs = init prob in
      printf "abs = %a" Abs.print abs;
      if not (Abs.is_bottom abs) then
        let cons = List.filter (fun exp -> not (is_cons_linear exp)) prob.constraints in
        (* Format.printf "\ncons = ["; *)
        (* List.iter (Format.printf "%a ;" (print_bexpr)) cons; *)
        (* Format.printf "]\n"; *)
        let (nb_steps, best_value, sols) = explore abs cons prob.objective prob.to_draw in
	Format.printf "solving ends\n%!";
        let nb_sol = List.length sols in
	match nb_sol with
	| 0 -> printf "No solutions - #created nodes: %d@." nb_steps
	| 1 -> printf "Unique solution - #created nodes: %d - best_value = %f@." nb_steps best_value
        | _ -> printf "#solutions: %d - #created nodes: %d - best_value = %f@." nb_sol nb_steps best_value
      else
        printf "No Solutions - #created nodes: 0@."
end
  
module Box = Minimize(Abstract_box.BoxF)
module BoxCP = Minimize(BoxCP)
module Oct = Minimize(OctBoxCP)
module Poly = Minimize(PolyCP)

module BoxNOct = Minimize(VariousDA.BoxNOct)
module BoxNPoly = Minimize(VariousDA.BoxNPoly)
module OctNPoly = Minimize(VariousDA.OctNPoly)
