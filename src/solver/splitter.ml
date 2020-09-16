open Adcp_sig
open Tools

(** Consistency computation and splitting strategy handling *)
module Make (Abs : AbstractCP) = struct

  include Boolean.Make(Abs)

  let init (problem:Csp.prog) : Abs.t =
    Csp.(List.fold_left (fun abs (t,v,d) ->
             let c = Csp_helper.domain_to_constraints (t,v,d) in
             let abs = Abs.add_var abs (t,v) in
             filter abs c
           )  Abs.empty problem.init)

  type consistency = Full of Abs.t * Csp.csts
		               | Maybe of Abs.t * Csp.ctrs * Csp.csts
		               | Empty

  let print_debug tab obj abs =
    if !Constant.debug >= 5 then
      match obj with
      | Some obj ->
         let (inf, sup) = Abs.forward_eval abs obj in
         Format.printf "%sabs = %a\tobjective = (%s, %s)@." tab Abs.print abs (Mpqf.to_string inf) (Mpqf.to_string sup)
      | None -> Format.printf "%sabs = %a@." tab Abs.print abs

  let print_debug_const tab cstrs csts =
    if !Constant.debug >= 5 then Format.printf "#constraints = %d@." (List.length cstrs)
  (*Format.printf "%sconstraints:\n" tab;
      List.iter (fun (c, j) -> Format.printf "%s%s%a\n" tab tab Csp.print_bexpr c) cstrs;
      Format.printf "%sconstants:\n" tab;
      List.iter (fun v -> Format.printf "%s%s%a\n" tab tab Csp.print_csts v) csts*)

  let minimize_test obj abs =
    match obj with
    | Some obj -> let (inf, sup) = Abs.forward_eval abs obj in inf = sup
    | None -> false

  (** This is the main propagation loop.
      Only one iteration is performed if `Constant.iter` equals `false`.
      Otherwise we iterate until we obtain a fixed point,
      or if a propagation step prunes less than a certain ratio.
  *)
  let rec consistency abs ?obj:objv (constrs:Csp.ctrs) (const:Csp.csts) : consistency =
    Tools.debug 2 "consistency\n%!";
    try
      let abs' = List.fold_left (fun a (c, _) -> filter a c) abs constrs in
      if Abs.is_empty abs' then Empty else
	      let unsat = List.filter (fun (c, _) -> not (sat_cons abs' c)) constrs in
	      match unsat with
	      | [] -> print_debug "\t=> sure:" objv abs'; Full (abs', const)
	      | _ ->  if minimize_test objv abs' then
                  (print_debug "\t*******=> sure:" objv abs'; Full (abs', const))
                else (
                  print_debug "\t=> " objv abs';
                  print_debug_const "\t  " unsat const;
                  let (abs'', unsat', const') = check_csts abs' unsat const in
                  match Abs.vars abs'' with
                  | [] -> print_debug "\t=> sure:" objv abs''; Full (abs'', const')
                  | _ -> (
                    print_debug_const "\t  " unsat' const';
                    if !Constant.iter then
                      let ratio = (Abs.volume abs'')/.(Abs.volume abs) in
                      if ratio > 0.9 || abs = abs'' then
                        Maybe(abs'', unsat', const')
                      else
                        consistency abs'' unsat' const'
                    else
                      Maybe(abs'', unsat', const')))
    with Bot.Bot_found -> if !Constant.debug > 1 then Format.printf "\t=> bot\n"; Empty

  (* using elimination technique *)
  let prune (abs:Abs.t) (constrs:Csp.ctrs) =
    Tools.debug 2 "pruning\n%!";
    match Abs.prune with
    | None -> [],[abs]
    | Some prune ->
       let rec aux abs c_list is_sure sures unsures =
         match c_list with
         | [] -> if is_sure then (abs::sures),unsures else sures,(abs::unsures)
         | h::tl ->
	          try
              let (c, _) = h in
	            let neg = Csp_helper.neg_bexpr c |> filter abs in
	            let s = prune abs neg in
              let u = Abs.meet abs neg in
	            let s',u' = List.fold_left (fun (sures,unsures) elm ->
	                            aux elm tl is_sure sures unsures)
	                          (sures,unsures) s
	            in
	            aux u tl false s' u'
	          with Bot.Bot_found -> aux abs tl is_sure sures unsures
       in aux abs constrs true [] []

  let get_value abs v e =
    let (lb, ub) = Abs.forward_eval abs e in
    let slope = max (Mpqf.abs lb) (Mpqf.abs ub) in
    let (xl, xu) = Abs.forward_eval abs (Csp.Var v) in
    let diam = Mpqf.sub xu xl in
    let value = Mpqf.mul slope diam in
    (value, Mpqf.div (Mpqf.add xu xl) (Mpqf.of_int 2))

  let max_smear abs (jacobian:Csp.ctrs) : Abs.t list =
    let (_, vsplit, mid) =
      List.fold_left (
          fun (m', mv', mid') (_, l) ->
          List.fold_left (
              fun (m, mv, mid) (v, e) ->
              let (value, half) = get_value abs v e in
              if m < value then (value, v, half)
              else (m, mv, mid)
            ) (m', mv', mid') l
        ) (Mpqf.of_int (-1), "", Mpqf.of_int (-1)) jacobian
    in
    [Abs.filter abs (Csp.Var vsplit, Csp.LEQ, Csp.Cst (mid, Csp.Real));
     Abs.filter abs (Csp.Var vsplit, Csp.GT, Csp.Cst (mid, Csp.Real))]

  let sum_smear abs (jacobian:Csp.ctrs) : Abs.t list =
    let smear =
      List.fold_left (
          fun map (_, l) ->
          List.fold_left (
              fun m (v, e) ->
              let (value, half) = get_value abs v e in
              match (VarMap.find_opt v m) with
              | None -> VarMap.add v (value, half) m
              | Some (s, _) -> VarMap.add v (Mpqf.add s value, half) m
            ) map l
        ) VarMap.empty jacobian
    in
    let (_, vsplit, mid) =
      VarMap.fold (
          fun var (smear, mi) (m, v, s) ->
          if smear > m then (smear, var, mi)
          else (m, v, s)
        ) smear (Mpqf.of_int (-1), "", Mpqf.of_int (-1))
    in
    [Abs.filter abs (Csp.Var vsplit, Csp.LEQ, Csp.Cst (mid, Csp.Real));
     Abs.filter abs (Csp.Var vsplit, Csp.GT, Csp.Cst (mid, Csp.Real))]

  let pizza_split (abs : Abs.t) (jacobian:Csp.ctrs) : Abs.t list =
    let abs' = (!Constant.precision *. 2. |> Mpqf.of_float)
      |> Abs.shrink abs
    in
    if Abs.is_empty abs'
    then Abs.split abs jacobian
    else begin
        let splits = begin
          let starting_point = Abs.spawn abs'
            |> VectorMap.RationalVec.map Mpqf.to_float
          and includes x = VectorMap.FloatVec.map Mpqf.of_float x
            |> Abs.is_abstraction abs'
          in
          match Gradient_descent.gradient_descent starting_point includes jacobian with
          | Some xs -> Abs.split_on abs jacobian
            (VectorMap.FloatVec.map Mpqf.of_float xs)
          | None -> Abs.split abs jacobian
          end
        in (* case where the pizza split has been made on a corner *)
        if List.length splits = 1
        then Abs.split abs jacobian
        else splits
    end

  let split abs =
    Tools.debug 1 "splitting using %s\n%!" !Constant.split;
    let splitting_strategy =
      match !Constant.split with
      | "maxSmear" -> max_smear
      | "smear" -> sum_smear
      | "pizza" -> pizza_split
      | _ -> Abs.split
    in
    splitting_strategy abs

end
