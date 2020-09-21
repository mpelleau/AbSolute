open Signature

module Minimize(Abs : AbstractCP) = struct

  include Splitter.Make(Abs)
  module Res = Result.Make(Abs)

  let splitting_strategy =
    match !Constant.split with
    | "maxSmear" -> max_smear
    | "smear" -> sum_smear
    | _ -> split

  let is_small a obj =
    let (obj_inf, obj_sup) = Abs.forward_eval a obj in
    Mpqf.to_float (Mpqf.sub obj_sup  obj_inf) <= !Constant.precision

  let explore abs constrs obj consts views splitting =
    let open Res in
    let rec aux abs cstrs obj csts res depth =
      match consistency abs ~obj:obj cstrs csts with
      | Empty -> res
      | Full (abs', const) -> add_s res ~obj:obj (abs', const, views)
      | Maybe(a,_,csts) when stop res a || is_small a obj -> add_u res ~obj:obj (a, csts, views)
      | Maybe(abs', cstrs, csts)  ->
         if !Constant.pruning && depth < !Constant.pruning_iter then
           let ls,lu = prune abs' cstrs in
           let res = List.fold_left (fun r x -> add_s r ~obj:obj (x, csts, views)) res ls in
           List.fold_left (fun res x ->
                List.fold_left (fun res elem ->
                     aux elem cstrs obj csts (incr_step res) (depth + 1)
                ) res (splitting x cstrs)
	   ) res lu
         else
           List.fold_left (fun res elem ->
                aux elem cstrs obj csts (incr_step res) (depth + 1)
	   ) res (splitting abs' cstrs) in
    aux abs constrs obj consts (empty_obj_res abs obj) 0

  let minimizing prob =
    let open Csp in
    let abs = init prob in
    Format.printf "abs = %a\tvolume = %f@." Abs.print abs (Abs.volume abs);
    let res =  explore abs prob.jacobian prob.objective prob.constants prob.view splitting_strategy in
    Format.printf "\noptimization ends\n%!%a" Res.print res;
    res
end
