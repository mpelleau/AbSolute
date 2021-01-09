(* open Signature
 *
 * module Minimize(Abs : AbstractCP) = struct
 *
 *   include Splitter.Make(Abs)
 *
 *   let splitting_strategy =
 *     match !Constant.split with
 *     (\* | "maxSmear" -> max_smear
 *      * | "smear" -> sum_smear *\)
 *     | _ -> split
 *
 *   let is_small a obj =
 *     let (obj_inf, obj_sup) = Abs.forward_eval a obj in
 *     Mpqf.to_float (Mpqf.sub obj_sup obj_inf) <= !Constant.precision
 *
 *   let explore abs constrs obj splitting =
 *     let rec aux abs cstrs obj res depth =
 *       match consistency abs cstrs with
 *       | Unsat -> res
 *       | Sat -> add_s res ~obj:obj abs
 *       | Filtered(a,_) when stop res a || is_small a obj -> add_u res ~obj:obj a
 *       | Filtered(abs', _)  ->
 *          List.fold_left (fun res elem ->
 *              aux elem cstrs obj (incr_step res) (depth + 1)
 * 	         ) res (splitting abs' []) in
 *     aux abs constrs obj (empty_obj_res abs obj) 0
 *
 *   let minimizing prob =
 *     let open Csp in
 *     let abs = init prob in
 *     Format.printf "abs = %a\tvolume = %f@." Abs.print abs (Abs.volume abs);
 *     let res =  explore abs prob.constraints prob.objective splitting_strategy in
 *     Format.printf "\noptimization ends\n%!%a" Result.print res;
 *     res
 * end *)
