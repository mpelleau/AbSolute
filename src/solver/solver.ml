open Adcp_sig
open ADCP

(* Solver *)
module Solve(Abs : AbstractCP) = struct

  include Splitter.Make(Abs)

  let explore (abs:Abs.t) (constrs:Csp.constrs) : Abs.t Result.t =
    let open Result in
    let rec aux abs cstrs res =
      match consistency abs cstrs with
      | Empty -> res
      | Full abs' -> add_s res abs'
      | Maybe(a,cstrs) when stop res a || Abs.is_small a -> add_u res a
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
    let res =  explore abs prob.Csp.constraints in
    Format.printf "\nsolving ends\n%!%a" Result.print res;
    res

  let solving_various prob =
    let open Csp in
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
    Format.printf "\nsolving ends\n%!%a" Result.print res;
    res
end
