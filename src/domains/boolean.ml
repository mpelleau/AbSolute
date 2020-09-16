(** This wrapper lifts the filtering and the satisfies function over arithmetical
    predicates (e1 < e2) to boolean formulas of the form (p1 \/ p2) *)

(** Boolean expressions abstractions *)
module Make (Abs:Signature.AbstractCP) = struct

  (** generic filter function over boolean formulas *)
  let rec filter (value:Abs.t) c =
    let open Csp in
    match c with
    | And (b1,b2) -> filter (filter value b2) b1
    | Or (b1,b2) ->
       let a1 = try Some(filter value b1) with Bot.Bot_found -> None
       and a2 = try Some(filter value b2) with Bot.Bot_found -> None in
       (match (a1,a2) with
        | (Some a1),(Some a2) -> Abs.join a1 a2
        | None, (Some x) | (Some x), None -> x
        | _ -> raise Bot.Bot_found)
    | Not b -> filter value (Csp_helper.neg_bexpr b)
    | Cmp (binop,e1,e2) -> Abs.filter value (e1,binop,e2)

  (** checks if an abstract satisfies a constraint, i.e all of the concrete
      instances abstracted by the element satisfy the constraint *)
  let sat_cons (a:Abs.t) (constr:Csp.bexpr) : bool =
    try Abs.is_empty (filter a (Csp_helper.neg_bexpr constr))
    with Bot.Bot_found -> true

  let check_csts (a:Abs.t) (constrs:Csp.ctrs) (const:Csp.csts) =
    let newc = Abs.bound_vars a in
    let tmp = Csp_helper.get_vars_jacob constrs in
    let ctrs = List.fold_left
                 (fun l (v, (a, _)) ->
                   Csp_helper.replace_cst_jacob (v, a) l
                 ) tmp newc in
    let newa = List.fold_left (fun a' (v, _) -> Abs.rem_var a' v) a newc in
    let (_, vars) = List.split (Abs.vars newa) in
    let ctrs_vars = List.fold_left
                      ( fun s (_, v, _) -> Csp_helper.Variables.union s v
                      ) Csp_helper.Variables.empty ctrs in
    let unconstrained = List.filter (fun v -> not (Csp_helper.Variables.mem v ctrs_vars)) vars in
    let v_unconst = List.map (fun v -> (v, Abs.var_bounds newa v)) unconstrained in
    let abs = List.fold_left (fun a' v -> Abs.rem_var a' v) newa unconstrained in

    let newctrs = List.map (fun (c, _, j) -> (c, j)) ctrs in
    (abs, newctrs, v_unconst@(newc@const))

end
