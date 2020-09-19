(* This module provides preprocessing utilities.
   It finds the constants in the CSP and the views
   in order to make the CSP easier to solve. *)

open Csp
open Csp_helper
open Rewrite

let get_csts csp =
  let (csts, vars) = List.partition
    (fun (_, _v, d) -> match d with
       | Finite (a, b) -> a=b
       | _ -> false
    ) csp.init in
  let cst = List.map
              (fun (_, v, d) ->
                match d with
                | Finite (a, b) when a=b -> (v, (a, b))
                | _ -> assert false
              ) csts in
  {csp with init = vars; constants = cst@csp.constants}


let get_vars_cstrs cstrs =
  List.map (fun c -> (c, get_vars_set_bexpr c)) cstrs

let replace_cst_cstrs (id, cst) cstrs =
  List.map (fun (c, vars) -> if Variables.mem id vars then
                               (replace_cst_bexpr (id, cst) c, Variables.remove id vars)
                             else (c, vars)
    ) cstrs

let replace_cst ctrs csts =
  List.fold_left
    (fun l (v, (a, b)) ->
      if a = b then replace_cst_cstrs (v, a) l
      else l
    ) ctrs csts


let rec get_cst_value expr c =
  match expr with
  | Binary (ADD, e1, e2) ->
     (match e1, e2 with
      | Cst a, e | e, Cst a -> (e, Mpqf.add c a)
      | e1, e2 ->
         let (e1', c1) = get_cst_value e1 c in
         let (e2', c2) = get_cst_value e2 c in
         (Binary (ADD, e1', e2'), Mpqf.add c1 c2)
     )
  | Binary (SUB, e1, e2) ->
     (match e1, e2 with
      | Cst a, e -> (Unary (NEG, e), Mpqf.add c a)
      | e, Cst a -> (e, Mpqf.sub c a)
      | e1, e2 ->
         let (e1', c1) = get_cst_value e1 c in
         let (e2', c2) = get_cst_value e2 c in
         (Binary (SUB, e1', e2'), Mpqf.sub c1 c2)
     )
  | _ -> (expr, c)


let rewrite_ctr (op, e1, e2) =
  let (_, e1', _) = rewrite (op, expand e1, expand e2) in
  let (e, cst) = get_cst_value e1' Q.zero in
  let neg_cst = if is_zero cst then cst else Q.neg cst in
  (e, cst, neg_cst)

let rewrite_constraint = function
  | Cmp (op, e1, e2) ->
     let (e, c, negc) = rewrite_ctr (op, e1, e2) in
     Cmp (op, e, Cst negc)
  | _ as c -> c

let filter_cstrs ctr_vars consts =
  List.fold_left (fun (cstr, csts, b) (c, v) ->
      if Variables.cardinal v = 1 then
        match c with
        | Cmp (EQ, e1, e2) ->
           ((* Format.printf "%a@." print_bexpr c; *)
            let (e, cst, negc) = rewrite_ctr (EQ, e1, e2) in
            (* Format.printf "%a ===== (%s) %s@." print_expr e (Mpqf.to_string cst) (Mpqf.to_string negc); *)
            match e with
            | Var var -> (cstr, (var, (negc, negc))::csts, true)
            | Unary(NEG, Var var) -> (cstr, (var, (cst, cst))::csts,  true)
            | Binary(MUL, Var var, Cst a) | Binary(MUL, Cst a, Var var) -> (cstr, (var, (Mpqf.div negc a, Mpqf.div negc a))::csts, true)
            | Unary(NEG, (Binary(MUL, Var var, Cst a))) |  Unary(NEG, (Binary(MUL, Cst a, Var var))) -> (cstr, (var, (Mpqf.div cst a, Mpqf.div cst a))::csts, true)
            | _ -> ((c, v)::cstr, csts, b))
        | _ -> ((c, v)::cstr, csts, b)
      else ((c, v)::cstr, csts, b)
    ) ([], consts, false) ctr_vars


let rec repeat ctr_vars csts =
  let ctrs = replace_cst ctr_vars csts in
  let (ctrs', csts', b) = filter_cstrs ctrs csts in
  if b then
    repeat ctrs' csts'
  else
    (ctrs', csts')

let rec view e1 e2 =
  match e1, e2 with
  | Var v, _ -> (v, e2)
  | _, Var v  -> (v, e1)
  | Unary (NEG, n), e -> view n (simplify_fp (Unary (NEG, e)))
  | Binary (ADD, Var v, a), e
    | Binary (ADD, a, Var v), e
    -> (v, simplify_fp (Binary (SUB, e, a)))
  | Binary (ADD, a1, a2), e when has_variable a1
    -> view a1 (simplify_fp (Binary (SUB, e, a2)))
  | Binary (ADD, a1, a2), e -> view a2 (simplify_fp (Binary (SUB, e, a1)))
  | Binary (SUB, Var v, s), e
    -> (v, simplify_fp (Binary (ADD, e, s)))
  | Binary (SUB, s, Var v), e
    -> (v, simplify_fp (Binary (SUB, s, e)))
  | Binary (SUB, s1, s2), e when has_variable s1
    -> view s1 (simplify_fp (Binary (ADD, e, s2)))
  | Binary (SUB, s1, s2), e
    -> view s2 (simplify_fp (Binary (SUB, s1, e)))
  | Binary (MUL, m1, m2), e when has_variable m1
    -> view m1 (simplify_fp (Binary (DIV, e, m2)))
  | Binary (MUL, m1, m2), e -> view m2 (simplify_fp (Binary(DIV, e, m1)))
  | Binary (DIV, d1, d2), e when has_variable d1
    -> view d1 (simplify_fp (Binary(MUL, e, d2)))
  | Binary (DIV, d1, d2), e -> view d2 (simplify_fp (Binary(MUL, e, d1)))
  | _, _ -> ("NOPE", Binary(SUB, e1, e2))

let rec replace_view_expr ((id, e) as view) expr =
  match expr with
  | Var v when v = id -> e
  | Unary (op, u) -> Unary (op, replace_view_expr view u)
  | Binary (op, b1, b2) -> Binary (op, replace_view_expr view b1, replace_view_expr view b2)
  | Funcall (f, l) -> Funcall (f, List.map (replace_view_expr view) l)
  | _ as expr -> expr

let rec replace_view_bexpr view = function
  | Cmp (op, e1, e2) -> Cmp(op, replace_view_expr view e1, replace_view_expr view e2)
  | And (b1, b2) -> And (replace_view_bexpr view b1, replace_view_bexpr view b2)
  | Or (b1, b2) -> Or (replace_view_bexpr view b1, replace_view_bexpr view b2)
  | Not b -> Not (replace_view_bexpr view b)

let rep_view_ctr ((id, e) as view) ctrs =
  List.map (fun (c, vars) -> if Variables.mem id vars then
                               (replace_view_bexpr view c,
                                Variables.union (Variables.remove id vars)
                                  (Variables.of_list (get_vars_expr e)))
                             else (c, vars)
    ) ctrs


let replace_view ctrs views =
  List.fold_left (fun l v -> rep_view_ctr v l) ctrs views

let rep_view view views =
  List.map (fun (id, e) -> (id, replace_view_expr view e)) views

let rep_in_view view views =
  List.fold_left (fun (id, e) v -> (id, replace_view_expr v e)) view views

let replace_view_ctr ctr views =
  let replaced = List.fold_left (fun c v -> replace_view_bexpr v c) ctr views in
  match replaced with
  | Cmp (_, e1', e2') -> e1',e2'
  | _ -> assert false


let rec aux_view ctrs views = function
  | [] -> (ctrs, views)
  | (Cmp (EQ, e1, e2) as c, v)::t when is_cons_linear c && Variables.cardinal v = 2 ->
     let (e, _, value) = rewrite_ctr (EQ, e1, e2) in
     let (tmp, tmp2) as new_view = rep_in_view (view e (Cst value)) views in
     let views' = rep_view new_view views in
     let t' = rep_view_ctr new_view t in
     aux_view ctrs (new_view::views') t'
  | (c, v)::t ->
     aux_view ((c, v)::ctrs) views t


let get_views ctr_vars =
  aux_view [] [] ctr_vars


let rec find_all_views ctrs =
  let (ctrs, vws) = get_views ctrs in
  let views = List.map (fun (id, e) -> (id, e)) vws in
  if List.length views > 0 then
    let ctrs' = replace_view ctrs views in
    let (v, c) = find_all_views ctrs' in
    (views@v, c)
  else
    ([], ctrs)


let to_domains (e, d) =
  match d with
  | Finite (l,h) -> [Cmp(GEQ, e, Cst l); Cmp(LEQ, e, Cst h)]
  | Minf (i) -> [Cmp(LEQ, e, Cst i)]
  | Inf (i) -> [Cmp(GEQ, e, Cst i)]
  | _ -> []


let is_cons_linear_eq = function
  | Cmp (EQ, _, _) as c when is_cons_linear c -> true
  | _ -> false

let get_nb_eq csp = List.length (List.filter (is_cons_linear) csp.constraints)


let no_views csp =
  let p = get_csts csp in
  let ctr_vars = get_vars_cstrs p.constraints in
  let (ctrs, csts) = repeat ctr_vars p.constants in

  let (ctrs, _) = List.split ctrs in
  let (cons, _) = List.split csts in

  let vars = List.filter (fun (t, v, d) -> not(List.mem v cons)) p.init in

  {p with init = vars; constants = csts; constraints = ctrs}




let rec preprocess csp =
  let nb_eq = get_nb_eq csp in

  let p = get_csts csp in
  let ctr_vars = get_vars_cstrs p.constraints in
  let (ctrs, csts) = repeat ctr_vars p.constants in



  let (views, ctrs') = find_all_views ctrs in

  let (ctrs, _) = List.split ctrs' in
  let (cons, _) = List.split csts in
  let (view, _) = List.split views in


  let (var_view, vars') = List.partition (fun (t, v, d) -> List.mem v view) p.init in

  let vars = List.filter (fun (t, v, d) -> not(List.mem v cons)) vars' in

  let view_ctrs = List.fold_left (
                      fun l (_, v, d) ->
                      let e = List.assoc v views in
                      List.append (to_domains (e, d)) l) [] var_view in

  let all_ctrs = ctrs@ view_ctrs in
  let all_views = csp.view@views in
  let prob = {p with init = vars; constants = csts; constraints = all_ctrs; view = all_views} in
  let nb_eq' = get_nb_eq prob in
  if nb_eq = nb_eq' then prob
  else preprocess prob
