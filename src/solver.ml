open Format
open Utils
open Adcp_sig
open ADCP

(* Solver *)
module Solve(Abs : AbstractCP) = struct

  include Splitter.Make(Abs)
  module Printer = Out.Make(Abs)

  type result = {
    sure : Abs.t list;             (* abstract elements that satisfy the constraints *)
    unsure : Abs.t list;           (* abstract elements that MAY satisfy the constraints *) 
    nb_sols : int;                 (* number of solutions *)
    nb_steps : int                 (* number of steps of the solving process *)
  }


  (* tests if a result can't be splitted anymore *)
  let stop res abs =  
    Abs.is_small abs 
    || res.nb_sols > !Constant.max_sol
    || res.nb_steps > !Constant.max_iter

  let explore (abs:Abs.t) (constrs:Syntax.constrs) : result =
    let rec aux abs cstrs res =
      match consistency abs cstrs with
      | Empty -> res
      | Full abs' -> {res with sure=(abs'::res.sure); nb_sols=res.nb_sols+1}
      | Maybe(abs',cstrs)  ->
	if stop res abs' then {res with unsure=(abs'::res.unsure); nb_sols=res.nb_sols+1}
	else if res.nb_sols <= !Constant.max_sol then
          List.fold_left (fun res elem -> 
	    aux elem cstrs {res with nb_steps=res.nb_steps+1}
	  ) res (split abs' cstrs)
        else res
    in aux abs constrs {sure=[]; unsure=[]; nb_sols=0; nb_steps=0}

  let explore_with_pruning (abs:Abs.t) (constrs:Syntax.constrs) : result =
    let add_to add base = List.fold_left (fun a b -> b::a) base add in
    let rec aux abs cstrs res =
      match consistency abs cstrs with
      | Empty -> res
      | Full abs' -> {res with sure=(abs'::res.sure); nb_sols=res.nb_sols+1}
      | Maybe(abs',cstrs')  ->
	if stop res abs' then {res with unsure=(abs'::res.unsure); nb_sols=res.nb_sols+1}
	else
	  let ls,lu = prune abs' cstrs' in
	  let res = {res with sure = add_to ls res.sure} in
	  List.fold_left (fun res x ->
            List.fold_left (fun res elem -> 
	      aux elem cstrs' {res with nb_steps=res.nb_steps+1}
	    ) res (split x cstrs')
	  ) res lu
    in aux abs constrs {sure=[]; unsure=[]; nb_sols=0; nb_steps=0}

  let solving prob =
    let open Syntax in
    let abs = Abs.of_problem prob in
    printf "abs = %a@." Abs.print abs;
    let exp = if !Constant.pruning |> not then explore else explore_with_pruning in
    let res =  exp abs prob.constraints in
    printf "\nsolving ends\n%!";
    if not (Abs.is_bottom abs) then
      match res.nb_sols with
      | 0 -> printf "No solutions - #created nodes: %d@." res.nb_steps
      | 1 -> printf "Unique solution - #created nodes: %d@." res.nb_steps
      | _ -> printf "#solutions: %d - #created nodes: %d@."res.nb_sols res.nb_steps
    else printf "No Solutions - #created nodes: 0@.";
    Printer.out res.sure res.unsure prob.to_draw
	
  let solving_various prob =
    let open Syntax in
    let abs = Abs.of_problem prob in
    printf "abs = %a" Abs.print abs;
    if not (Abs.is_bottom abs) then
      let cons = List.filter (fun exp -> not (is_cons_linear exp)) prob.constraints in
      printf "\nconstraints = [";
      List.iter (Format.printf "%a ;" (print_bexpr)) prob.constraints;
      printf "]@.";
      printf "non linear constraints = [";
      List.iter (Format.printf "%a ;" (print_bexpr)) cons;
      printf "]@.";
      let res = explore abs cons in
      Printer.out res.sure res.unsure prob.to_draw;
      printf "solving ends\n%!";
      match res.nb_sols with
      | 0 -> printf "No solutions - #created nodes: %d@." res.nb_steps
      | 1 -> printf "Unique solution - #created nodes: %d@." res.nb_steps
      | _ -> printf "#solutions: %d - #created nodes: %d@." res.nb_sols res.nb_steps
    else
      printf "No Solutions - #created nodes: 0@."
end

(************ INSTANCES ************)

(* single domain *)
module Box = Solve(Abstract_box.BoxF)
module BoxCP = Solve(BoxCP)
module Oct = Solve(OctBoxCP)
module Poly = Solve(PolyCP)

(* reduced product *)
module BoxNOct = Solve(VariousDA.BoxNOct)
module BoxNPoly = Solve(VariousDA.BoxNPoly)
module OctNPoly = Solve(VariousDA.OctNPoly)
