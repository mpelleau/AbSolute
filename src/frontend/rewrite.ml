(* This module provides constraint rewriting utilities.
   It is able to perform simplification over multivariate polynoms
   and multiple occurences of a same expression *)
open Csp
open Tools

module CoEnv = Map.Make(struct type t = expr let compare = compare end)
module PI    = Polynom.Rational

let reverse_map (m1 : string CoEnv.t) : expr VarMap.t =
  CoEnv.fold (fun k v env ->
      VarMap.add v k env
    ) m1 VarMap.empty

exception Empty

(* special variables begin with a '%' character to avoid name clash *)
let fresh_name =
  let cpt = ref 0 in
  fun () ->
  incr cpt;
  "%reserved_"^(string_of_int (!cpt))

(* We convert a tree expression to a polynomial form.
   sub-expression does not correspond to a polynom are bound to fresh
   variables.
   for example:
   2*x^2 + cos(x) + (cos(x))^2=y
   is rewritten into :
   2*x^2 + %reserved_1 + %reserved_1^2 = y
*)
let rec simplify env expr : (PI.t * string CoEnv.t) =
  let check_var e env =
    try
      let var = CoEnv.find e env in
      (PI.of_var var),env
    with Not_found ->
      let new_var = fresh_name() in
      let newenv = CoEnv.add e new_var env in
      (PI.of_var new_var),newenv
  in
  let p,env =
  match expr with
  | Var v -> (PI.of_var v),env
  | Cst (c,_) -> (PI.of_rational c),env
  | Binary (op,e1,e2) ->
     (*Format.printf "===== %a, %a@." print_expr e1 print_expr e2;*)
     let p1,env' = simplify env e1 in
     let p2,env'' = simplify env' e2 in
     (*Format.printf "!!!!! %a, %a@." print_expr (polynom_to_expr p1 env') print_expr (polynom_to_expr p2 env'');*)
     (match op with
      | ADD -> (*Format.printf "§§§§§ %a@." print_expr (polynom_to_expr (PI.add p1 p2) env'');*) (PI.add p1 p2),env''
      | SUB -> (*Format.printf "§§§§§ %a@." print_expr (polynom_to_expr (PI.sub p1 p2) env'');*) (PI.sub p1 p2),env''
      | MUL -> (*Format.printf "§§§§§ %a@." print_expr (polynom_to_expr (PI.mul p1 p2) env'');*) (PI.mul p1 p2),env''
      | DIV ->
         (* only division by a constant to make sure we do not shadow any dbz *)
         (match PI.div p1 p2 with
         | Some p -> p,env''
         | None ->
            let e1 = polynom_to_expr p1 env''
            and e2 = polynom_to_expr p2 env'' in
            let e = Binary (DIV,e1,e2) in
            check_var e env'')
      | POW ->
         (* only constant exponentiation *)
         (match PI.pow p1 p2 with
         | Some p -> p,env''
         | None ->
         let e1 = polynom_to_expr p1 env'' and e2 = polynom_to_expr p2 env'' in
         let e = Binary (POW,e1,e2) in
         check_var e env''))
  | Unary (u,e) ->
     let p,env = simplify env e in
     (match u with
      | NEG -> (PI.neg p),env)
  | Funcall(name,args) ->
     let p_args,env' =
       List.fold_left (fun (pargs,env) arg ->
           let p,e = simplify env arg in
           (p::pargs,e)
         ) ([],env) args
     in
     let args' = List.rev_map (fun p -> polynom_to_expr p env') p_args in
     let e = Funcall (name, args') in
     check_var e env'
  in (PI.clean p),env

(* polynom to expression conversion *)
and polynom_to_expr (p:PI.t) (fake_vars: string CoEnv.t) : Csp.expr =
  let fake_vars = reverse_map fake_vars in
  let of_id id =
    try VarMap.find id fake_vars
    with Not_found -> Var id
  in
  let var_to_expr ((id,exp):PI.var) : expr =
    let rec iter acc = function
      | 0 -> acc
      | n -> iter (Binary(MUL,acc,(of_id id))) (n-1)
    in
    match (PI.to_int exp) with
    | 0 -> one
    | n -> iter (of_id id) (n - 1)
  in
  let cell_to_expr ((c,v) as m) =
    let c = PI.to_rational c in
    if PI.is_monom_constant m then Cst (c,Real)
    else if Mpqf.equal c (Mpqf.of_int 1) then
      match v with
      | h::tl -> List.fold_left (fun acc e ->
          Binary(MUL,acc,(var_to_expr e))
        ) (var_to_expr h) tl
      | _ -> assert false
    else
      List.fold_left (fun acc e ->
          Binary(MUL,acc,(var_to_expr e))
        ) (Cst (c,Real)) v
  in
  match p with
  | [] -> zero
  | h::tl -> List.fold_left (fun acc c -> Binary(ADD,acc,(cell_to_expr c)))
                            (cell_to_expr h)
                            tl

(* simplify the polynomial part of a constraint *)
let rewrite (cmp,e1,e2) : (cmpop * expr * expr) =
  (* we move e2 to left side to perform potentially more simplifications *)
  let e1', e2' = (expand e1, expand e2) in
  (* Format.printf "---------- %a = %a@." print_expr e1' print_expr e2'; *)
  let p1, env1 = simplify CoEnv.empty e1' in
  let p2, env2 = simplify env1 e2' in
  (* Format.printf "********** %a = %a@." print_expr (polynom_to_expr p1 env1) print_expr (polynom_to_expr p2 env2); *)
  let polynom = PI.clean (PI.sub p1 p2) in
  let simplified_left = polynom_to_expr polynom env2 in
  (* Format.printf "$$$$$$$$$$ %a = 0@." print_expr simplified_left; *)
  let simp_left = Csp.simplify_fp simplified_left in
  (* Format.printf "&&&&&&&&&& %a = 0@." print_expr simp_left; *)
  let e2 = zero in
  (cmp,simp_left,e2)


(* rewriting main function *)
let rewrite_csp (p:Csp.prog) : Csp.prog =
  let res = List.map (Csp.map_constr rewrite) p.constraints in
  {p with constraints = res}
