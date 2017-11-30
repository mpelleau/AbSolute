open Adcp_sig
open ADCP

(* Solver *)
module Solve(Abs : AbstractCP) = struct

  include Splitter.Make(Abs)
  module Res = Result.Make(Abs)

  let splitting_strategy abs jacobian =
    match !Constant.split with
    | "default" -> split abs jacobian
    | "maxSmear" -> max_smear abs jacobian
    | "smear" -> sum_smear abs jacobian

  let explore (abs:Abs.t) (constrs:Csp.ctrs) =
    let open Res in
    let rec aux abs cstrs res depth =
      match consistency abs cstrs with
      | Empty -> res
      | Full abs' -> add_s res abs'
      | Maybe(a,cstrs) when stop res a -> res
      | Maybe(a,cstrs) when Abs.is_small a -> add_u res a
      | Maybe(abs',cstrs) ->
         if !Constant.pruning && depth < !Constant.pruning_iter then
           let ls,lu = prune abs' cstrs in
           let res = List.fold_left (fun r x -> add_s r x) res ls in
           List.fold_left (fun res x ->
               List.fold_left (fun res elem ->
                   aux elem cstrs (incr_step res) (depth +1)
                 ) res (splitting_strategy x cstrs)
	           ) res lu
         else
           List.fold_left (fun res elem ->
             aux elem cstrs (incr_step res) (depth +1)
           ) res (splitting_strategy abs' cstrs)
    in aux abs constrs empty_res 0

  let solving prob =
    let abs = init prob in
    Format.printf "abs = %a\tvolume = %f\n@." Abs.print abs (Abs.volume abs);
    let res =  explore abs prob.Csp.jacobian in
    Format.printf "\nsolving ends\n%!%a" Res.print res;
    res

  let solving_various prob =
    let open Csp in
    let abs = init prob in
    let cons = List.filter (fun (e, _) -> not (is_cons_linear e)) prob.jacobian in
    let lcons = List.filter (fun (e, _) -> (is_cons_linear e)) prob.jacobian in
    let abs = List.fold_left (fun a (c, _) -> filterl a c) abs lcons in
    Format.printf "abs = %a@." Abs.print abs;
    let res = explore abs cons in
    Format.printf "\nsolving ends\n%!%a" Res.print res;
    res
end
