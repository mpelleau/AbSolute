(** This wrapper internalizes the representation on the constraint to make
    it domain dependant. It allows to permform transformations on the problem
    at initialization *)

(** Boolean expressions abstractions *)
module Boolean (Abs:Adcp_sig.AbstractCP) = struct

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
    | Not b -> filter value (neg_bexpr b)
    | Cmp (binop,e1,e2) -> Abs.filter value (e1,binop,e2)

  let sat_cons (a:Abs.t) (constr:Csp.bexpr) : bool =
    let open Csp in
    try Abs.is_empty (filter a (neg_bexpr constr))
    with Bot.Bot_found -> true

  let check_csts (a:Abs.t) (constrs:Csp.ctrs) (const:Csp.csts) =
    let newc = Abs.bound_vars a in
    let tmp = Csp.get_vars_jacob constrs in
    let ctrs = List.fold_left
                 (fun l (v, (a, _)) ->
                   Csp.replace_cst_jacob (v, a) l
                 ) tmp newc in
    let newa = List.fold_left (fun a' (v, _) -> Abs.rem_var a' v) a newc in
    let (_, vars) = List.split (Abs.vars newa) in
    let ctrs_vars = List.fold_left
                      ( fun s (_, v, _) -> Csp.Variables.union s v
                      ) Csp.Variables.empty ctrs in
    let unconstrained = List.filter (fun v -> not (Csp.Variables.mem v ctrs_vars)) vars in
    let v_unconst = List.map (fun v -> (v, Abs.var_bounds newa v)) unconstrained in
    let abs = List.fold_left (fun a' v -> Abs.rem_var a' v) newa unconstrained in

    let newctrs = List.map (fun (c, _, j) -> (c, j)) ctrs in
    (abs, newctrs, v_unconst@(newc@const))

end

module Make(D:Adcp_sig.AbstractCP) = struct
  include Boolean(D)

  type t = {
      search_space  : D.t;
      constraints   : Csp.constrs;
    }

  let init (prob:Csp.prog) =
    let search_space = List.fold_left (fun acc (ann,name,_) ->
                           D.add_var acc (ann,name)) D.empty prob.Csp.init
    in
    let constraints = prob.Csp.constraints in
    {search_space; constraints}

  (** returns the variables annoted by their type *)
  let vars {search_space; _} = D.vars search_space

  (** returns the bounds of a variable *)
  let var_bounds {search_space; _} = D.var_bounds search_space

  (** returns the bound variables *)
  let bound_vars {search_space; _} = D.bound_vars search_space

  (** removes an unconstrained variable from the environnement *)
  let rem_var {search_space; _} = D.rem_var search_space

  (*** PREDICATES ***)

  (** tests if an abstract element is small enough with respect to `Constant.precision` *)
  let is_small {search_space; _} = D.is_small search_space

  (** tests if an abstract element is empty *)
  let is_empty {search_space; _} = D.is_empty search_space


  (** splits an abstract element *)
  let split {search_space; constraints} ctrs =
    List.rev_map (fun sp -> {search_space=sp; constraints}) (D.split search_space ctrs)

  (** Pizza splits an abstract element around the given point *)
  let split_on {search_space; constraints} ctrs instance =
    List.rev_map (fun sp -> {search_space=sp; constraints})
      (D.split_on search_space instance ctrs)

  (** filters an abstract element *)
  let filter {search_space; constraints} =
    let abs' = List.fold_left (fun a c -> filter a c) search_space constraints in
    {search_space=abs'; constraints}

  (** returns the range of value of a given expression for an abstract element *)
  let forward_eval {search_space; _} =  D.forward_eval search_space

  (** transforms an abstract element into constraints *)
  let to_bexpr {search_space; _} = D.to_bexpr search_space

  (** printing *)
  let print fmt {search_space; _} : unit = D.print fmt search_space

  (** computes the volume of an abstract element *)
  let volume {search_space; _} = D.volume search_space

  (** concretization function. we call it a spawner.
      useful to do tests, and to reuse the results.
      values are generated randomly *)
  let spawn {search_space; _} = D.spawn search_space

  (** check if an abstract element is an abstraction of an instance *)
  let is_abstraction {search_space; _} = D.is_abstraction search_space

  (** Skrinks the abstract element in every direction by the given value. *)
  let shrink {search_space; _} = D.shrink search_space

end
