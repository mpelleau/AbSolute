open Adcp_sig

(* Solver *)
module Solve(Abs : AbstractCP) = struct

  include Splitter.Make(Abs)
  include Result.Make(Abs)

  (* main propagation loop *)
  let explore (abs:Abs.t) (constrs:Csp.ctrs) (consts:Csp.csts) (views:Csp.jacob) =
    Tools.debug 1 "entering the solving loop\n%!";
    let rec aux abs cstrs csts res depth =
      match consistency abs cstrs csts with
      | Empty -> res
      | Full (abs', const) -> add_s res (abs', const, views)
      | Maybe(a, cstrs, csts) when stop res a || Abs.is_small a -> add_u res (a, csts, views)
      | Maybe(abs', cstrs, csts) ->
         if !Constant.pruning && depth < !Constant.pruning_iter then
           let ls,lu = prune abs' cstrs in
           let res = List.fold_left (fun r x -> add_s r (x, csts, views)) res ls in
           List.fold_left (fun res x ->
               List.fold_left (fun res elem ->
                   aux elem cstrs csts (incr_step res) (depth +1)
                 ) res (split x cstrs)
	     ) res lu
         else
           List.fold_left (fun res elem ->
             aux elem cstrs csts (incr_step res) (depth +1)
           ) res (split abs' cstrs)
    in aux abs constrs consts empty_res 0

  let solving prob =
    let abs = init prob in
    explore abs prob.Csp.jacobian prob.Csp.constants prob.Csp.view
end
