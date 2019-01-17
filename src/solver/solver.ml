open Adcp_sig

(* Solver *)
module Solve(Abs : AbstractCP) = struct

  include Splitter.Make(Abs)
  include Result.Make(Abs)

  let explore (abs:Abs.t) (constrs:Csp.ctrs) (consts:Csp.csts)
   (views:Csp.jacob) (enter_node: unit -> unit) =
    (* propagation/exploration loop *)
    let rec aux cstrs csts depth res abs =
      enter_node ();
      match consistency abs cstrs csts with
      | Empty -> res
      | Full (abs', const) -> add_s res (abs', const, views)
      | Maybe(a, _, csts) when stop res a || Abs.is_small a -> add_u res (a, csts, views)
      | Maybe(abs', cstrs, csts) ->
         List.fold_left (fun res elem ->
             aux cstrs csts (depth +1) (incr_step res) elem
           ) res (split abs' cstrs)
    in
    (* propagation/elimination/exploration loop *)
    let rec aux_elim cstrs csts depth res abs =
      enter_node ();
      match consistency abs cstrs csts with
      | Empty -> res
      | Full (abs', const) -> add_s res (abs', const, views)
      | Maybe(a, _, csts) when stop res a || Abs.is_small a -> add_u res (a, csts, views)
      | Maybe(abs', cstrs, csts) ->
         if depth < !Constant.pruning_iter then
           let ls,lu = prune abs' cstrs in
           let res = List.fold_left (fun r x -> add_s r (x, csts, views)) res ls in
           List.fold_left (fun res x ->
               List.fold_left (fun res elem ->
                   aux_elim cstrs csts (depth +1) (incr_step res) elem
                 ) res (split x cstrs)
             ) res lu
         else
           List.fold_left (fun res elem ->
               aux cstrs csts (depth +1) (incr_step res) elem
             ) res (split abs' cstrs)
    in
    (if !Constant.pruning then aux_elim else aux)
      constrs consts 0 empty_res abs

  let solving_instrumented prob enter_node =
    Tools.debug 1 "entering the resolution\n";
    let abs = init prob in
    let res = explore abs prob.Csp.jacobian prob.Csp.constants prob.Csp.view enter_node in
    res

  (* entry point of the solver *)
  let solving prob =
    solving_instrumented prob (fun () -> ())
end
