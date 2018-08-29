open Adcp_sig

(* Solver *)
module Solve(Abs : AbstractCP) = struct

  include Splitter.Make(Abs)
  include Result.Make(Abs)

  let splitting_strategy =
    match !Constant.split with
    | "maxSmear" -> max_smear
    | "smear" -> sum_smear
    | _ -> split

  let explore (abs:Abs.t) (constrs:Csp.ctrs) (consts:Csp.csts) (views:Csp.jacob) splitting =
    if !Constant.debug > 0 then Tools.debug 0 "entering the solving loop\n%!";
    let rec aux abs cstrs csts res depth =
      match consistency abs cstrs csts with
      | Empty -> res
      | Full (abs', const) -> add_s res (abs', const, views)
      | Maybe(a, cstrs, csts) when stop res a -> add_u res (a, csts, views)
      | Maybe(a, cstrs, csts) when Abs.is_small a -> add_u res (a, csts, views)
      | Maybe(abs', cstrs, csts) ->
         if !Constant.pruning && depth < !Constant.pruning_iter then
           let ls,lu = prune abs' cstrs in
           let res = List.fold_left (fun r x -> add_s r (x, csts, views)) res ls in
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
    let res = explore abs prob.Csp.jacobian prob.Csp.constants prob.Csp.view splitting_strategy in
    Format.printf "\nsolving ends\n%!%a" print res;
    res
end
