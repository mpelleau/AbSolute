open Apron
open Mpqf
open Format
open Utils
open ADCP

module Minimize(Abs : AbstractCP) =
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
      in cons_loop abs 0

    let rec explore abs env tab obj min_obj minabs max_iter prec nb_steps =
      let abs' = consistency abs tab max_iter in
      if Abstract1.is_bottom man abs' then
        (* No solutions in this sub-tree. *)
        (nb_steps, min_obj, minabs)
      else
        (
        (*let box = Abstract1.to_box man abs' in
        let itv = box.Abstract1.interval_array in
        printf "@.box = %a@.min = %f@." (print_array Interval.print) itv (scalar_to_float min_obj);*)
        let objitv = Abstract1.bound_texpr man abs' obj in
        let objinf = objitv.Interval.inf in
        (*let objsup = objitv.Interval.sup in
        printf "branch obj = [|%f, %f|]@." (scalar_to_float objinf) (scalar_to_float objsup);*)
        if (Scalar.cmp objinf min_obj) > 0 then
          (
          (* There's no point in searching in this branch 
          printf "No point in keeping on searching!@.";*)
          (nb_steps, min_obj, minabs)
          )
        else
          (
          (* Keep on searching in this sub-tree. *)
          let (small, exprs) = Abs.is_small abs' prec in
          if small then
            (* Solution found! *)
            (
            (* let box = Abstract1.to_box man abs' in
            printf "obj = %f@." (scalar_to_float objinf);
            print_sol box;*)
            if (Scalar.cmp objinf min_obj) < 0 then
              (nb_steps, objinf, [abs'])
            else
              (nb_steps, objinf, (List.append minabs [abs']))
            )
          else
            (
            (* Split the next variable, heuristic 
            printf "Not a solution -> splitting.@.";*)
            let list_abs = Abs.split abs' exprs in
            List.fold_left (fun (nbe, mobj, labs) absi -> explore absi env tab obj mobj labs max_iter prec (nbe+1)) (nb_steps, min_obj, minabs) list_abs
            )
          )
        )
    let minimizing env domains cons obj =
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
        (
        let objitv = Abstract1.bound_texpr man abs obj in
        let objinf = objitv.Interval.inf in
        let objsup = objitv.Interval.sup in
        printf "branch obj = [|%f, %f|]@." (scalar_to_float objinf) (scalar_to_float objsup);
        let (nb_steps, min_obj, labs) = explore abs env cons obj objsup [abs] max_iter prec 1 in
      
        if (List.length labs) == 0 then
          printf "No solutions - #created nodes: %d@." nb_steps
        else
          if (List.length labs) == 1 then
            (
            printf "Unique solution - #created nodes: %d@." nb_steps;
            let sol = List.hd labs in
            let box = Abstract1.to_box man sol in
            printf "obj = %f@." (scalar_to_float min_obj);
            print_sol box;
            )
          else
            (
            printf "#solutions: %d - #created nodes: %d@.obj = %f@." (List.length labs) nb_steps (scalar_to_float min_obj);
            List.iter (fun absi -> 
            let box = Abstract1.to_box man absi in
            print_sol box;) labs;
            )
        )
      else
        printf "No Solutions - #created nodes: 0@."

    let minimizing problem = 
      let (env, domains, andor, constraints, objective) = problem in
      minimizing env domains constraints objective

  end

    
module OctBox = Minimize(OctBoxCP)
module OctMinMax = Minimize(OctMinMaxCP)
module OctMinMin = Minimize(OctMinMinCP)

