open Apron
open Mpqf
open Format
open Utils
open ADCP

module Minimize(Abs : AbstractCP) =
  struct

    let consistency abs tab =
      let abs' = List.fold_left Abs.meet abs tab in
      (if Abs.is_bottom abs' then `Empty 
       else if List.for_all (Abs.sat_cons abs') tab then `Full
       else `Maybe)
       ,abs'

    let explore abs constrs obj =
      let rec aux abs best_value nb_steps sols =
	let cons,abs' = consistency abs constrs in
	match cons with
	| `Empty -> (nb_steps, best_value, sols)
	| `Full | `Maybe  ->
          let (obj_value, _) = Abs.forward_eval abs' obj in
          if obj_value > best_value then
            (* There's no point in keep on searching in this branch *)
            (nb_steps, best_value, sols)
          else
            (match (Abs.is_small abs' !Constant.precision) with
	    | true,_ ->
              if obj_value < best_value then
                (nb_steps, obj_value, [abs'])
              else
                (nb_steps, obj_value, abs'::sols)
	    | _,exprs when (List.length sols) <= !Constant.max_sol ->
              Abs.split abs' exprs |>
              List.fold_left (fun (a, b, c) d -> aux d b (a+1) c) (nb_steps, best_value, sols)
        | _ -> (nb_steps, best_value, sols)
	)
      in 
      let (_, obj_sup) = Abs.forward_eval abs obj in
      let res = aux abs obj_sup 1 [] in 
      res

    let minimizing prob =
      let open Syntax in
      let abs = Abs.of_problem prob in
      printf "abs = %a@." Abs.print abs;
      if not (Abs.is_bottom abs) then
        let (nb_steps, best_value, sols) = explore abs prob.constraints prob.objective in
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
      let abs = Abs.of_problem prob in
      printf "abs = %a" Abs.print abs;
      if not (Abs.is_bottom abs) then
        let cons = List.filter (fun exp -> not (is_cons_linear exp)) prob.constraints in
        (* Format.printf "\ncons = ["; *)
        (* List.iter (Format.printf "%a ;" (print_bexpr)) cons; *)
        (* Format.printf "]\n"; *)
        let (nb_steps, best_value, sols) = explore abs cons prob.objective in
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

