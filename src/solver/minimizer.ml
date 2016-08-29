open Format
open ADCP
open Adcp_sig

module Minimize(Abs : AbstractCP) = struct

  include Splitter.Make(Abs)

  type result = {
    sure : Abs.t list;             (* abstract elements that satisfy the constraints *)
    unsure : Abs.t list;           (* abstract elements that MAY satisfy the constraints *)
    best_value : float;            (* best value found *)
    nb_sols : int;                 (* number of solutions *)
    nb_steps : int                 (* number of steps of the solving process *)
  }

  (* tests if a result can't be splitted anymore *)
  let stop res abs =
    Abs.is_small abs
    || res.nb_sols > !Constant.max_sol
    || res.nb_steps > !Constant.max_iter

  let explore abs constrs obj =
    let rec aux abs cstrs obj res =
      match consistency abs cstrs with
      | Empty -> res
      | Full abs' ->
        let (obj_value, _) = Abs.forward_eval abs' obj in
        if obj_value > res.best_value then
	  res
        else if obj_value < res.best_value then
	  {sure=[abs']; unsure=[]; best_value=obj_value; nb_sols=1; nb_steps=res.nb_steps}
        else
	  {res with sure=(abs'::res.sure); nb_sols=res.nb_sols+1}
      | Maybe(abs',cstrs)  ->
	if stop res abs' then
	  (let (obj_value, _) = Abs.forward_eval abs' obj in
          if obj_value > res.best_value then
	    res
          else if obj_value < res.best_value then
	    {sure=[]; unsure=[abs']; best_value=obj_value; nb_sols=1; nb_steps=res.nb_steps}
          else
	    {res with unsure=(abs'::res.unsure); nb_sols=res.nb_sols+1})
	else if res.nb_sols <= !Constant.max_sol then
          List.fold_left (fun res elem ->
	    aux elem cstrs obj {res with nb_steps=res.nb_steps+1}
	  ) res (split abs' cstrs)
        else res
    in let (_, obj_sup) = Abs.forward_eval abs obj in
    aux abs constrs obj {sure=[]; unsure=[]; best_value=obj_sup; nb_sols=0; nb_steps=0}

  let explore_sure abs constrs obj =
    let rec aux abs cstrs obj res =
      match consistency abs cstrs with
      | Empty -> res
      | Full abs' ->
        let (obj_value, _) = Abs.forward_eval abs' obj in
        if obj_value > res.best_value then
	  res
        else if obj_value < res.best_value then
	  {sure=[abs']; unsure=[]; best_value=obj_value; nb_sols=1; nb_steps=res.nb_steps}
        else
	  {res with sure=(abs'::res.sure); nb_sols=res.nb_sols+1}
      | Maybe(abs',cstrs)  ->
	if not (stop res abs') && res.nb_sols <= !Constant.max_sol then
          List.fold_left (fun res elem ->
	    aux elem cstrs obj {res with nb_steps=res.nb_steps+1}
	  ) res (split abs' cstrs)
        else res
    in let (_, obj_sup) = Abs.forward_eval abs obj in
    aux abs constrs obj {sure=[]; unsure=[]; best_value=obj_sup; nb_sols=0; nb_steps=0}

  let minimizing prob =
    let open Csp in
    let abs = init prob in
    printf "abs = %a@." Abs.print abs;
    let exp = if !Constant.sure |> not then explore else explore_sure in
    let res =  exp abs prob.constraints prob.objective in
    (* let res =  explore abs prob.constraints prob.objective in *)
    printf "\nsolving ends\n%!";
    if not (Abs.is_bottom abs) then
      match res.nb_sols with
      | 0 -> printf "No solutions - #created nodes: %d@." res.nb_steps
      | 1 -> printf "Unique solution - #created nodes: %d@." res.nb_steps
      | _ -> printf "#solutions: %d - #created nodes: %d@."res.nb_sols res.nb_steps
    else printf "No Solutions - #created nodes: 0@."

    let minimizing_various prob =
    let open Csp in
    let abs = init prob in
    printf "abs = %a" Abs.print abs;
    if not (Abs.is_bottom abs) then
      let cons = List.filter (fun exp -> not (is_cons_linear exp)) prob.constraints in
      printf "\nconstraints = [";
      List.iter (Format.printf "%a ;" (print_bexpr)) prob.constraints;
      printf "]@.";
      printf "non linear constraints = [";
      List.iter (Format.printf "%a ;" (print_bexpr)) cons;
      printf "]@.";
      (* let res = explore abs prob.constraints prob.objective in *)
      let exp = if !Constant.sure |> not then explore else explore_sure in
      let res =  exp abs prob.constraints prob.objective in
      printf "solving ends\n%!";
      match res.nb_sols with
      | 0 -> printf "No solutions - #created nodes: %d@." res.nb_steps
      | 1 -> printf "Unique solution - #created nodes: %d@." res.nb_steps
      | _ -> printf "#solutions: %d - #created nodes: %d@." res.nb_sols res.nb_steps
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
