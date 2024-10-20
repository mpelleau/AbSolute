(* This module provides constraint rewriting utilities. It is able to perform
   simplification over multivariate polynoms and multiple occurences of a same
   expression *)
open Csp
open Tools

module CoEnv = Map.Make (struct
  type t = Expr.t

  let compare = compare
end)

module PI = Polynom.Rational

let reverse_map (m1 : string CoEnv.t) : Expr.t VarMap.t =
  CoEnv.fold (fun k v env -> VarMap.add v k env) m1 VarMap.empty

exception Empty

(* special variables begin with a '%' character to avoid name clash *)
let fresh_name =
  let cpt = ref 0 in
  fun () ->
    incr cpt ;
    "%reserved_" ^ string_of_int !cpt

(* We convert a tree expression to a polynomial form. sub-expression does not
   correspond to a polynom are bound to fresh variables. for example: 2*x^2 +
   cos(x) + (cos(x))^2=y is rewritten into : 2*x^2 + %reserved_1 + %reserved_1^2
   = y *)
let rec simplify env expr : PI.t * string CoEnv.t =
  let check_var e env =
    try
      let var = CoEnv.find e env in
      (PI.of_var var, env)
    with Not_found ->
      let new_var = fresh_name () in
      let newenv = CoEnv.add e new_var env in
      (PI.of_var new_var, newenv)
  in
  let p, env =
    let open Expr in
    match expr with
    | Var v -> (PI.of_var v, env)
    | Cst c -> (PI.of_rational c, env)
    | Binary (op, e1, e2) -> (
        let p1, env' = simplify env e1 in
        let p2, env'' = simplify env' e2 in
        match op with
        | ADD -> (PI.add p1 p2, env'')
        | SUB -> (PI.sub p1 p2, env'')
        | MUL -> (PI.mul p1 p2, env'')
        | DIV ->
            (* TODO: handle division by a constant to make sure we do not shadow
               any dbz *)
            let e1 = polynom_to_expr p1 env''
            and e2 = polynom_to_expr p2 env'' in
            let e = Binary (DIV, e1, e2) in
            check_var e env''
        | POW ->
            (* TODO: constant exponentiation *)
            let e1 = polynom_to_expr p1 env''
            and e2 = polynom_to_expr p2 env'' in
            let e = Binary (POW, e1, e2) in
            check_var e env'' )
    | Neg e ->
        let p, env = simplify env e in
        (PI.neg p, env)
    | Funcall (name, args) ->
        let p_args, env' =
          List.fold_left
            (fun (pargs, env) arg ->
              let p, e = simplify env arg in
              (p :: pargs, e))
            ([], env) args
        in
        let args' = List.rev_map (fun p -> polynom_to_expr p env') p_args in
        let e = Funcall (name, args') in
        check_var e env'
  in
  (PI.clean p, env)

(* polynom to expression conversion *)
and polynom_to_expr (p : PI.t) (fake_vars : string CoEnv.t) : Expr.t =
  let fake_vars = reverse_map fake_vars in
  let of_id id = try VarMap.find id fake_vars with Not_found -> Var id in
  let var_to_expr ((id, exp) : PI.var) : Expr.t =
    let rec iter acc = function
      | 0 -> acc
      | n -> iter (Expr.mul acc (of_id id)) (n - 1)
    in
    match exp with 0 -> Expr.one | n -> iter (of_id id) (n - 1)
  in
  let cell_to_expr ((c, v) as m) =
    let c = PI.to_rational c in
    if PI.is_monom_constant m then Expr.Cst c
    else if Mpqf.equal c (Mpqf.of_int 1) then
      match v with
      | h :: tl ->
          List.fold_left
            (fun acc e -> Expr.mul acc (var_to_expr e))
            (var_to_expr h) tl
      | _ -> assert false
    else List.fold_left (fun acc e -> Expr.mul acc (var_to_expr e)) (Cst c) v
  in
  match p with
  | [] -> Expr.zero
  | h :: tl ->
      List.fold_left
        (fun acc c -> Expr.add acc (cell_to_expr c))
        (cell_to_expr h) tl

(* simplify the polynomial part of a constraint *)
let rewrite (e1, cmp, e2) : Constraint.comparison =
  (* we move e2 to left side to perform potentially more simplifications *)
  let e1', e2' = (Csp_helper.expand e1, Csp_helper.expand e2) in
  let p1, env1 = simplify CoEnv.empty e1' in
  let p2, env2 = simplify env1 e2' in
  let polynom = PI.clean (PI.sub p1 p2) in
  let simplified_left = polynom_to_expr polynom env2 in
  let simp_left = Csp_helper.simplify_fp simplified_left in
  let e2 = Expr.zero in
  (simp_left, cmp, e2)

let rewrite_csp (p : Csp.t) : Csp.t =
  let res = List.map (Csp_helper.map_constr rewrite) p.constraints in
  {p with constraints= res}
