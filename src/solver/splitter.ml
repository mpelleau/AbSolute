open Signature
open Consistency

(** Consistency computation and splitting strategy handling *)
module Make (Abs : AbstractCP) = struct

  include Boolean.Make(Abs)

  let init (problem:Csp.prog) : Abs.t =
    Csp.(List.fold_left (fun abs (t,v,d) ->
             let c = Csp_helper.domain_to_constraints (t,v,d) in
             let abs = Abs.add_var abs (t,v) in
             match filter abs c with
             | Sat -> abs
             | Unsat -> failwith "invalid domain"
             | Filtered (abs,_) -> abs
           )  Abs.empty problem.init)

  let print_debug tab obj a =
    if !Constant.debug >= 5 then
      match obj with
      | Some obj ->
         let (l, u) = Abs.forward_eval a obj in
         Format.printf "%sabs = %a\tobjective = (%a, %a)@." tab Abs.print a Q.print l Q.print u
      | None -> Format.printf "%sabs = %a@." tab Abs.print a

  let print_debug_const cstrs =
    if !Constant.debug >= 5 then Format.printf "#constraints = %d@." (List.length cstrs)

  let minimize_test obj abs =
    match obj with
    | Some obj -> let (inf, sup) = Abs.forward_eval abs obj in inf = sup
    | None -> false

  (* filtering constraints in turn only once *)
  let consistency abs constr : Abs.t Consistency.t =
    let rec loop sat acc abs = function
      | [] -> Filtered (abs,sat)
      | c::tl ->
         (match filter abs c with
          | Sat -> loop sat acc abs tl
          | Unsat -> Unsat
          | Filtered (abs,true) -> loop sat acc abs tl
          | Filtered (abs,false) -> loop false (c::acc) abs tl)
    in
    loop true [] abs constr

  (* using elimination technique *)
  let prune (abs:Abs.t) (_:Csp.ctrs) = [],[abs]
    (* Tools.debug 2 "pruning\n%!";
     * match Abs.prune with
     * | None -> [],[abs]
     * | Some prune ->
     *    let rec aux abs c_list is_sure sures unsures =
     *      match c_list with
     *      | [] -> if is_sure then (abs::sures),unsures else sures,(abs::unsures)
     *      | h::tl ->
	   *         try
     *           let (c, _) = h in
	   *           let neg = Csp_helper.neg_bexpr c |> filter abs in
	   *           let s = prune abs neg in
     *           let u = Abs.meet abs neg in
	   *           let s',u' = List.fold_left (fun (sures,unsures) elm ->
	   *                           aux elm tl is_sure sures unsures)
	   *                         (sures,unsures) s
	   *           in
	   *           aux u tl false s' u'
	   *         with Bot.Bot_found -> aux abs tl is_sure sures unsures
     *    in aux abs constrs true [] [] *)

  let get_value abs v e =
    let (lb, ub) = Abs.forward_eval abs e in
    let slope = max (Mpqf.abs lb) (Mpqf.abs ub) in
    let (xl, xu) = Abs.forward_eval abs (Csp.Var v) in
    let diam = Mpqf.sub xu xl in
    let value = Mpqf.mul slope diam in
    (value, Mpqf.div (Q.add xu xl) Q.two)

  (* let max_smear abs (jacobian:Csp.ctrs) : Abs.t list =
   *   let (_, vsplit, mid) =
   *     List.fold_left (
   *         fun (m', mv', mid') (_, l) ->
   *         List.fold_left (
   *             fun (m, mv, mid) (v, e) ->
   *             let (value, half) = get_value abs v e in
   *             if m < value then (value, v, half)
   *             else (m, mv, mid)
   *           ) (m', mv', mid') l
   *       ) (Q.minus_one, "", Q.minus_one) jacobian
   *   in
   *   [Abs.filter abs (Csp.Var vsplit, Csp.LEQ, Csp.Cst mid);
   *    Abs.filter abs (Csp.Var vsplit, Csp.GT, Csp.Cst mid)] *)

  (* let sum_smear abs (jacobian:Csp.ctrs) : Abs.t list =
   *   let smear =
   *     List.fold_left (
   *         fun map (_, l) ->
   *         List.fold_left (
   *             fun m (v, e) ->
   *             let (value, half) = get_value abs v e in
   *             match (VarMap.find_opt v m) with
   *             | None -> VarMap.add v (value, half) m
   *             | Some (s, _) -> VarMap.add v (Mpqf.add s value, half) m
   *           ) map l
   *       ) VarMap.empty jacobian
   *   in
   *   let (_, vsplit, mid) =
   *     VarMap.fold (
   *         fun var (smear, mi) (m, v, s) ->
   *         if smear > m then (smear, var, mi)
   *         else (m, v, s)
   *       ) smear (Q.minus_one, "", Q.minus_one)
   *   in
   *   [Abs.filter abs (Csp.Var vsplit, Csp.LEQ, Csp.Cst mid);
   *    Abs.filter abs (Csp.Var vsplit, Csp.GT, Csp.Cst mid)] *)

  let split abs =
    Tools.debug 1 "splitting using %s\n%!" !Constant.split;
    let splitting_strategy =
      match !Constant.split with
      (* | "maxSmear" -> max_smear
       * | "smear" -> sum_smear *)
      | _ -> Abs.split
    in
    splitting_strategy abs

end
