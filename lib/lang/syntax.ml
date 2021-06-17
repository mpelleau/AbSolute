module VSet = Set.Make (struct
  type t = Csp.decl

  let compare = compare
end)

let single_int v inf sup = VSet.singleton (Csp.Int, v, Dom.of_ints inf sup)

let single_float v inf sup = VSet.singleton (Csp.Real, v, Dom.of_floats inf sup)

let fresh =
  let cpt = ref 0 in
  fun () ->
    incr cpt ;
    Format.asprintf "x_%i" !cpt

let ( let$ ) (dom : int list) (f : VSet.t Expr.annot_t -> 'a) =
  match dom with
  | [inf; sup] ->
      let v = fresh () in
      f (Expr.AVar v, single_int v inf sup)
  | _ -> invalid_arg "let$"

let ( let@ ) dom (f : VSet.t Expr.annot_t -> 'a) =
  match dom with
  | [inf; sup] ->
      let v = fresh () in
      f (Expr.AVar v, single_float v inf sup)
  | _ -> invalid_arg "let@"

let ( * ) ((_, s1) as e1) ((_, s2) as e2) =
  (Expr.ABinary (MUL, e1, e2), VSet.union s1 s2)

let ( / ) ((_, s1) as e1) ((_, s2) as e2) =
  (Expr.ABinary (DIV, e1, e2), VSet.union s1 s2)

let ( + ) ((_, s1) as e1) ((_, s2) as e2) =
  (Expr.ABinary (ADD, e1, e2), VSet.union s1 s2)

let ( - ) ((_, s1) as e1) ((_, s2) as e2) =
  (Expr.ABinary (SUB, e1, e2), VSet.union s1 s2)

let ( ^ ) ((_, s1) as e1) ((_, s2) as e2) =
  (Expr.ABinary (POW, e1, e2), VSet.union s1 s2)

let cmp op ((_, s1) as e1) ((_, s2) as e2) =
  Constraint.Cmp ((e1, op, e2), VSet.union s1 s2)

let ( > ) e1 e2 = cmp Constraint.GT e1 e2

let ( < ) e1 e2 = cmp Constraint.LT e1 e2

let ( >= ) e1 e2 = cmp Constraint.LEQ e1 e2

let ( <= ) e1 e2 = cmp Constraint.GEQ e1 e2

let ( = ) e1 e2 = cmp Constraint.EQ e1 e2

let ( ~. ) x = (Expr.ACst (Q.of_int x), VSet.empty)

let rec get_vars =
  let open Constraint in
  function
  | Cmp (_, s) -> s
  | Not c -> get_vars c
  | Or (c1, c2) | And (c1, c2) -> VSet.union (get_vars c1) (get_vars c2)

let rec deannot_constr =
  let open Constraint in
  function
  | Cmp ((e1, cmp, e2), _) -> Cmp (Expr.deannot e1, cmp, Expr.deannot e2)
  | Not c -> Not (deannot_constr c)
  | Or (c1, c2) -> Or (deannot_constr c1, deannot_constr c2)
  | And (c1, c2) -> And (deannot_constr c1, deannot_constr c2)

let build cstrs =
  let vset =
    List.fold_left (fun acc c -> get_vars c |> VSet.union acc) VSet.empty cstrs
  in
  let cstrs = List.rev_map deannot_constr cstrs in
  let csp = Csp.initialize (VSet.elements vset) in
  List.fold_left Csp.add_constr csp cstrs

let prob =
  let@ x = [-1000.; 1000.] in
  let c1 = (~.3 * x) ^ (~.2 + (~.4 * x) - ~.5) = ~.0 in
  let c2 = x <= ~.500 in
  build [c1; c2]
