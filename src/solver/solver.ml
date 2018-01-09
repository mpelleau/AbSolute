open Adcp_sig
open ADCP

(* Solver *)
module Solve(Abs : AbstractCP) = struct

  include Splitter.Make(Abs)
  module Res = Result.Make(Abs)

  let splitting_strategy =
    match !Constant.split with
    | "maxSmear" -> max_smear
    | "smear" -> sum_smear
    | _ -> split

  let explore (abs:Abs.t) (constrs:Csp.ctrs) (consts:Csp.csts) splitting =
    let open Res in
    let rec aux abs cstrs csts res depth =
      match consistency abs cstrs csts with
      | Empty -> res
      | Full (abs', const) -> add_s res (abs', const)
      | Maybe(a, cstrs, csts) when stop res a -> res
      | Maybe(a, cstrs, csts) when Abs.is_small a -> add_u res (a, csts)
      | Maybe(abs', cstrs, csts) ->
         if !Constant.pruning && depth < !Constant.pruning_iter then
           let ls,lu = prune abs' cstrs in
           let res = List.fold_left (fun r x -> add_s r (x, csts)) res ls in
           List.fold_left (fun res x ->
               List.fold_left (fun res elem ->
                   aux elem cstrs csts (incr_step res) (depth +1)
                 ) res (splitting x cstrs)
	           ) res lu
         else
           List.fold_left (fun res elem ->
             aux elem cstrs csts (incr_step res) (depth +1)
           ) res (splitting abs' cstrs)
    in aux abs constrs consts empty_res 0

  let solving prob =
    let abs = init prob in
    Format.printf "abs = %a\tvolume = %f\n@." Abs.print abs (Abs.volume abs);
    let res =  explore abs prob.Csp.jacobian prob.Csp.constants splitting_strategy in
    Format.printf "\nsolving ends\n%!%a" Res.print res;
    res

  let solving_various prob =
    let open Csp in
    let abs = init prob in
    let cons = List.filter (fun (e, _) -> not (is_cons_linear e)) prob.jacobian in
    let lcons = List.filter (fun (e, _) -> (is_cons_linear e)) prob.jacobian in
    let abs = List.fold_left (fun a (c, _) -> filterl a c) abs lcons in
    Format.printf "abs = %a@." Abs.print abs;
    let res = explore abs cons prob.constants splitting_strategy in
    Format.printf "\nsolving ends\n%!%a" Res.print res;
    res
end
