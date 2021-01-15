(* This module provides preprocessing utilities. It finds the constants in the
   CSP and the views in order to make the CSP easier to solve. *)

open Csp_helper
open Rewrite
open Tools

let get_vars_cstrs cstrs = List.map (fun c -> (c, get_vars_set_bexpr c)) cstrs

let replace_cst_cstrs (id, cst) cstrs =
  List.map
    (fun (c, vars) ->
      if VarSet.mem id vars then
        (replace_cst_bexpr (id, cst) c, VarSet.remove id vars)
      else (c, vars))
    cstrs

let replace_cst ctrs csts =
  List.fold_left
    (fun l (v, (a, b)) -> if a = b then replace_cst_cstrs (v, a) l else l)
    ctrs csts

let rec get_cst_value expr c =
  let open Expr in
  match expr with
  | Binary (ADD, e1, e2) -> (
    match (e1, e2) with
    | Cst a, e | e, Cst a -> (e, Mpqf.add c a)
    | e1, e2 ->
        let e1', c1 = get_cst_value e1 c in
        let e2', c2 = get_cst_value e2 c in
        (Binary (ADD, e1', e2'), Mpqf.add c1 c2) )
  | Binary (SUB, e1, e2) -> (
    match (e1, e2) with
    | Cst a, e -> (Neg e, Mpqf.add c a)
    | e, Cst a -> (e, Mpqf.sub c a)
    | e1, e2 ->
        let e1', c1 = get_cst_value e1 c in
        let e2', c2 = get_cst_value e2 c in
        (Binary (SUB, e1', e2'), Mpqf.sub c1 c2) )
  | _ -> (expr, c)

let rewrite_ctr (e1, op, e2) =
  let e1', _, _ = rewrite (expand e1, op, expand e2) in
  let e, cst = get_cst_value e1' Q.zero in
  let neg_cst = if is_zero cst then cst else Q.neg cst in
  (e, cst, neg_cst)

let rewrite_constraint = function
  | Constraint.Cmp (e1, op, e2) ->
      let e, _, negc = rewrite_ctr (e1, op, e2) in
      Constraint.Cmp (e, op, Expr.Cst negc)
  | _ as c -> c

let rec replace_view_expr ((id, e) as view) expr =
  let open Expr in
  match expr with
  | Var v when v = id -> e
  | Neg u -> Neg (replace_view_expr view u)
  | Binary (op, b1, b2) ->
      Binary (op, replace_view_expr view b1, replace_view_expr view b2)
  | Funcall (f, l) -> Funcall (f, List.map (replace_view_expr view) l)
  | _ as expr -> expr

let rec replace_view_bexpr view =
  let open Constraint in
  function
  | Cmp (e1, op, e2) ->
      Cmp (replace_view_expr view e1, op, replace_view_expr view e2)
  | And (b1, b2) -> And (replace_view_bexpr view b1, replace_view_bexpr view b2)
  | Or (b1, b2) -> Or (replace_view_bexpr view b1, replace_view_bexpr view b2)
  | Not b -> Not (replace_view_bexpr view b)

let rep_view_ctr ((id, e) as view) ctrs =
  List.map
    (fun (c, vars) ->
      if VarSet.mem id vars then
        ( replace_view_bexpr view c
        , VarSet.union (VarSet.remove id vars)
            (VarSet.of_list (get_vars_expr e)) )
      else (c, vars))
    ctrs

let replace_view ctrs views =
  List.fold_left (fun l v -> rep_view_ctr v l) ctrs views

let rep_view view views =
  List.map (fun (id, e) -> (id, replace_view_expr view e)) views

let rep_in_view view views =
  List.fold_left (fun (id, e) v -> (id, replace_view_expr v e)) view views

(* let no_views csp =
 *   let p = get_csts csp in
 *   let ctr_vars = get_vars_cstrs p.constraints in
 *   let (ctrs, csts) = repeat ctr_vars  in
 *   let (ctrs, _) = List.split ctrs in
 *   let (cons, _) = List.split csts in
 *   let vars = List.filter (fun (_, v, _) -> not(List.mem v cons)) p.init in
 *   {p with init = vars; constants = csts; constraints = ctrs} *)

(* let rec preprocess csp =
 *   let nb_eq = get_nb_eq csp in
 *   let p = get_csts csp in
 *   let ctr_vars = get_vars_cstrs p.constraints in
 *   let (ctrs, csts) = repeat ctr_vars p.constants in
 *   let (views, ctrs') = find_all_views ctrs in
 *   let (ctrs, _) = List.split ctrs' in
 *   let (cons, _) = List.split csts in
 *   let (view, _) = List.split views in
 *   let (var_view, vars') = List.partition (fun (_, v, _) -> List.mem v view) p.init in
 *   let vars = List.filter (fun (_, v, _) -> not(List.mem v cons)) vars' in
 *   let view_ctrs = List.fold_left (
 *                       fun l (_, v, d) ->
 *                       let e = List.assoc v views in
 *                       List.append (to_domains (e, d)) l) [] var_view in
 *   let all_ctrs = ctrs@ view_ctrs in
 *   let prob = {p with init = vars; constants = csts; constraints = all_ctrs;} in
 *   let nb_eq' = get_nb_eq prob in
 *   if nb_eq = nb_eq' then prob
 *   else preprocess prob *)
