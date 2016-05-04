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

  type result = {
    values : (Abs.t * bool) list;  (* the abstract values. true for full, false otherwise *) 
    nb_sols : int;                 (* number of solutions *)
    nb_steps : int                 (* number of steps of the solving process *)
  }

  let is_small abs = Abs.is_small abs !Constant.precision

  let explore (abs:Abs.t) (constrs:Syntax.constrs) : result =
    let rec aux abs cstrs res =
      match consistency abs cstrs with
      | Empty -> res
      | Full abs' -> {res with values=((abs',true)::res.values); nb_sols=res.nb_sols+1}	
      | Maybe(abs',cstrs)  ->
	let (small,expr) = is_small abs' in
	if small then {res with values=((abs',false)::res.values); nb_sols=res.nb_sols+1}
	else if res.nb_sols <= !Constant.max_sol then
          List.fold_left (fun res elem -> 
	    aux elem cstrs {res with nb_steps=res.nb_steps+1}
	  ) res (split abs' expr)
        else res
    in aux abs constrs {values=[];nb_sols=0;nb_steps=0}

  module Printer = Out.Make(Abs)
      
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

module Box = Solve(Abstract_box.BoxF)
module BoxCP = Solve(BoxCP)
module Oct = Solve(OctBoxCP)
module Poly = Solve(PolyCP)

module BoxNOct = Solve(VariousDA.BoxNOct)
module BoxNPoly = Solve(VariousDA.BoxNPoly)
module OctNPoly = Solve(VariousDA.OctNPoly)
