open Utils
open Adcp_sig
open ADCP

(* Solver *)
module Solve(Abs : AbstractCP) = struct

  include Splitter.Make(Abs)
  module Printer = Out.Make(Abs)

  (* the result type we'll be manipulating *)
  type result = {
    sure      : Abs.t list;   (* elements that satisfy the constraints *)
    unsure    : Abs.t list;   (* elements that MAY satisfy the constraints *)
    nb_sure   : int;          (* size of sure list *)
    nb_unsure : int;          (* size of unsure list *)
    nb_steps  : int           (* number of steps of the solving process *)
  }

  (* the initialisation result *)
  let empty_res = {
    sure      = [];
    unsure    = [];
    nb_sure   = 0;
    nb_unsure = 0;
    nb_steps  = 0
  }

  (* adds an unsure element to a result *)
  let add_u res u =
    {res with unsure=(u::res.unsure); nb_unsure=res.nb_unsure+1}

  (* adds a sure element to a result *)
  let add_s res s =
    {res with sure=(s::res.sure); nb_sure=res.nb_sure+1}

  (* tests if a result can't be splitted anymore *)
  let stop res abs =
    Abs.is_small abs
    || res.nb_sure + res.nb_unsure > !Constant.max_sol
    || res.nb_steps > !Constant.max_iter

  (* prints a result *)
  let printf res =
    Format.printf "#solutions: %d\n#created nodes: %d%!\n"
      (if !Constant.sure then res.nb_sure
      else res.nb_sure+res.nb_unsure)
      res.nb_steps

  let explore (abs:Abs.t) (constrs:Syntax.constrs) : result =
    let rec aux abs cstrs res =
      match consistency abs cstrs with
      | Empty -> res
      | Full abs' -> add_s res abs'
      | Maybe(abs',cstrs) when stop res abs' -> add_u res abs'
      | Maybe(abs',cstrs) ->
         if !Constant.pruning then
	         let ls,lu = prune abs' cstrs in
	         let res = {res with sure = List.rev_append ls res.sure} in
	         List.fold_left (fun res x ->
             List.fold_left (fun res elem ->
	             aux elem cstrs {res with nb_steps=res.nb_steps+1}
	           ) res (split x cstrs)
	         ) res lu
         else
           List.fold_left (fun res elem ->
	           aux elem cstrs {res with nb_steps=res.nb_steps+1}
	         ) res (split abs' cstrs)
    in aux abs constrs empty_res

  let solving prob =
    let abs = init prob in
    Format.printf "abs = %a@." Abs.print abs;
    let res =  explore abs prob.Syntax.constraints in
    Format.printf "\nsolving ends\n%!";
    printf res;
    Printer.out res.sure res.unsure prob.Syntax.to_draw

  let solving_various prob =
    let open Syntax in
    let abs = init prob in
    Format.printf "abs = %a" Abs.print abs;
    let cons = List.filter (fun e -> not (is_cons_linear e)) prob.constraints in
    Format.printf "\nconstraints = [";
    List.iter (Format.printf "%a ;" (print_bexpr)) prob.constraints;
    Format.printf "]@.";
    Format.printf "non linear constraints = [";
    List.iter (Format.printf "%a ;" (print_bexpr)) cons;
    Format.printf "]@.";
    let res = explore abs cons in
    Printer.out res.sure res.unsure prob.to_draw;
    Format.printf "solving ends\n%!";
    printf res
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
