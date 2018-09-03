open Adcp_sig
open Tools

(* Boolean expressions abstractions *)
module Boolean (Abs:AbstractCP) = struct

  let rec filter (value:Abs.t) c =
    let open Csp in
    (* Format.printf "%a@." print_bexpr c;*)
    match c with
    | And (b1,b2) -> filter (filter value b2) b1
    | Or (b1,b2) ->
       let a1 = try Some(filter value b1) with Bot.Bot_found -> None
       and a2 = try Some(filter value b2) with Bot.Bot_found -> None in
       (match (a1,a2) with
        | (Some a1),(Some a2) -> Abs.join a1 a2
        | None, (Some x) | (Some x), None -> x
        | _ -> raise Bot.Bot_found)
    | Not b -> filter value (neg_bexpr b)
    | Cmp (binop,e1,e2) -> Abs.filter value (e1,binop,e2)

  let sat_cons (a:Abs.t) (constr:Csp.bexpr) : bool =
    let open Csp in
    (* match constr with
    | Or (b1,b2) -> sat_cons a b1 || sat_cons a b2
    | And (b1,b2) -> sat_cons a b1 && sat_cons a b2
    | Not b -> sat_cons a (neg_bexpr b)
    | _ -> *)
    try Abs.is_empty (filter a (neg_bexpr constr))
    with Bot.Bot_found -> true

  let check_csts (a:Abs.t) (constrs:Csp.ctrs) (const:Csp.csts) =
    let newc = Abs.bound_vars a in

    let tmp = Csp.get_vars_jacob constrs in
    let ctrs = List.fold_left
                 (fun l (v, (a, b)) ->
                   Csp.replace_cst_jacob (v, a) l
                 ) tmp newc in
    let newa = List.fold_left (fun a' (v, i) -> Abs.rem_var a' v) a newc in

    let (_, vars) = List.split (Abs.vars newa) in
    let ctrs_vars = List.fold_left
                      ( fun s (c, v, j) -> Csp.Variables.union s v
                      ) Csp.Variables.empty ctrs in
    let unconstrained = List.filter (fun v -> not (Csp.Variables.mem v ctrs_vars)) vars in
    let v_unconst = List.map (fun v -> (v, Abs.var_bounds newa v)) unconstrained in
    let abs = List.fold_left (fun a' v -> Abs.rem_var a' v) newa unconstrained in

    let newctrs = List.map (fun (c, _, j) -> (c, j)) ctrs in
    (abs, newctrs, v_unconst@(newc@const))

end

(* Consistency computation and splitting strategy handling *)
module Make (Abs : AbstractCP) = struct

  include Boolean(Abs)

  let init (problem:Csp.prog) : Abs.t =
    Csp.(List.fold_left (fun abs (t,v,d) ->
             let c = domain_to_constraints (t,v,d) in
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

  let rec consistency abs ?obj:objv (constrs:Csp.ctrs) (const:Csp.csts) : consistency =
    Tools.debug 1 "consistency\n%!";
    print_debug "" objv abs;
    print_debug_const "" constrs const;
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
    Tools.debug 1 "pruning\n%!";
    let rec aux abs c_list is_sure sures unsures =
      match c_list with
      | [] -> if is_sure then (abs::sures),unsures else sures,(abs::unsures)
      | h::tl ->
	       try
           let (c, _) = h in
	         let neg = Csp.neg_bexpr c |> filter abs in
	         let s,u = Abs.prune abs neg in
	         let s',u' = List.fold_left (fun (sures,unsures) elm ->
	                         aux elm tl is_sure sures unsures)
	                       (sures,unsures) s
	         in
	         aux u tl false s' u'
	       with Bot.Bot_found -> aux abs tl is_sure sures unsures
    in aux abs constrs true [] []

  let split abs cstrs =
    Tools.debug 1 "splitting\n%!";
    Abs.split abs
  (* TODO: add other splits *)

  let get_value abs v e =
    let (lb, ub) = Abs.forward_eval abs e in
    let slope = max (Mpqf.abs lb) (Mpqf.abs ub) in
    let (xl, xu) = Abs.forward_eval abs (Csp.Var v) in
    let diam = Mpqf.sub xu xl in
    let value = Mpqf.mul slope diam in
    (value, Mpqf.div (Mpqf.add xu xl) (Mpqf.of_int 2))

  let max_smear abs (jacobian:Csp.ctrs) : Abs.t list =
    let (msmear, vsplit, mid) = List.fold_left (
                                    fun (m', mv', mid') (_, l) ->
                                    List.fold_left (
                                        fun (m, mv, mid) (v, e) ->
                                        let (value, half) = get_value abs v e in
                                        if m < value then (value, v, half)
                                        else (m, mv, mid)
                                      ) (m', mv', mid') l
                                  ) (Mpqf.of_int (-1), "", Mpqf.of_int (-1)) jacobian
    in
    [Abs.filter abs (Csp.Var vsplit, Csp.LEQ, Csp.Cst (mid, Csp.Real)); Abs.filter abs (Csp.Var vsplit, Csp.GT, Csp.Cst (mid, Csp.Real))]

  module Smear = VarMap

  let sum_smear abs (jacobian:Csp.ctrs) : Abs.t list =
    let smear = List.fold_left (
                    fun map (_, l) ->
                    List.fold_left (
                        fun m (v, e) ->
                        let (value, half) = get_value abs v e in
                        match (Smear.find_opt v m) with
                        | None -> Smear.add v (value, half) m
                        | Some (s, _) -> Smear.add v (Mpqf.add s value, half) m
                      ) map l
                  ) Smear.empty jacobian
    in
    let (msmear, vsplit, mid) =
      Smear.fold (
          fun var (smear, mi) (m, v, s) ->
          if smear > m then (smear, var, mi)
          else (m, v, s)
        ) smear (Mpqf.of_int (-1), "", Mpqf.of_int (-1))
    in
    [Abs.filter abs (Csp.Var vsplit, Csp.LEQ, Csp.Cst (mid, Csp.Real)); Abs.filter abs (Csp.Var vsplit, Csp.GT, Csp.Cst (mid, Csp.Real))]
end
