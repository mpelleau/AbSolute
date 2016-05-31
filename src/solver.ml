open Format
open Utils
open Adcp_sig
open ADCP

(* Solver *)
module Solve(Abs : AbstractCP) = struct

  include Splitter.Make(Abs)
  module Printer = Out.Make(Abs)

  type result = {
    values : (Abs.t * bool) list;  (* the abstract values. true for full, false for maybe *) 
    nb_sols : int;                 (* number of solutions *)
    nb_steps : int                 (* number of steps of the solving process *)
  }

  let is_small abs = Abs.is_small abs !Constant.precision

  let explore (abs:Abs.t) (constrs:Syntax.constrs) : result =
    let rec aux abs cstrs res =
      match consistency abs cstrs with
      | Empty -> res
      | Full abs' -> 
	 (
	   if !Constant.trace then
	     printf "abs = %a@." Abs.print abs';
	   {res with values=((abs',true)::res.values); nb_sols=res.nb_sols+1}
	 )
      | Maybe(abs',cstrs)  ->
	let (small,expr) = is_small abs' in
	if small then 
	  (
	    if !Constant.trace then
	      printf "abs = %a@." Abs.print abs';
	    {res with values=((abs',false)::res.values); nb_sols=res.nb_sols+1}
	  )
	else if res.nb_sols <= !Constant.max_sol then
          List.fold_left (fun res elem -> 
	    aux elem cstrs {res with nb_steps=res.nb_steps+1}
	  ) res (split abs' expr)
        else res
    in aux abs constrs {values=[]; nb_sols=0; nb_steps=0}

  let solving prob =
    let open Syntax in
    let abs = Abs.of_problem prob in
    printf "abs = %a@." Abs.print abs;
    let res =  explore abs prob.constraints in
    Format.printf "solving ends\n%!";
    if not (Abs.is_bottom abs) then
      match res.nb_sols with
      | 0 -> printf "No solutions - #created nodes: %d@." res.nb_steps
      | 1 -> printf "Unique solution - #created nodes: %d@." res.nb_steps
      | _ -> printf "#solutions: %d - #created nodes: %d@."res.nb_sols res.nb_steps
    else printf "No Solutions - #created nodes: 0@.";
    Printer.out res.values prob.to_draw
	
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
      let res = explore abs cons in
      Printer.out res.values prob.to_draw;
      Format.printf "solving ends\n%!";
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
