open Tools

(* numeric expressions *)
type expr =
  | Neg of expr
  | Binary of Expr.binop * expr * expr
  | Var of string
  | Array of string * int
  | Cst of Q.t

(* boolean expressions *)
type bexpr =
  | Cmp of expr * Constraint.cmpop * expr
  | And of bexpr * bexpr
  | Or of bexpr * bexpr
  | Not of bexpr

module Env = VarMap

type env = {params: expr Env.t; paramsarray: expr array Env.t}

let set_param env p e = {env with params= Env.add p e env.params}

let param env p = Env.find p env.params

let name v i = v ^ string_of_int i

let param_i env p i =
  let arr = Env.find p env.paramsarray in
  arr.(i)

let get env name i =
  try param_i env name i with Not_found -> failwith ("cantfind array " ^ name)

(*replaces the param by the expression they represent*)
let rec substitute env = function
  | Neg (Cst i) -> Cst (Mpqf.neg i)
  | Neg e -> Neg (substitute env e)
  | Binary (b, e1, e2) ->
      let e1' = substitute env e1 and e2' = substitute env e2 in
      Binary (b, e1', e2')
  | Var v -> ( try param env v |> substitute env with Not_found -> Var v )
  | Array (v, i) -> (
    try param_i env v i |> substitute env with Not_found -> Var (name v i) )
  | Cst i -> Cst i

let substitute_constr env =
  let rec subs = function
    | Cmp (e1, c, e2) -> Cmp (substitute env e1, c, substitute env e2)
    | And (b1, b2) -> And (subs b1, subs b2)
    | Or (b1, b2) -> Or (subs b1, subs b2)
    | Not b -> Not (subs b)
  in
  subs

(* force evaluation of some expressions *)
let evaluate env expr =
  let binary = function
    | Expr.ADD -> Mpqf.add
    | Expr.SUB -> Mpqf.sub
    | Expr.DIV -> Mpqf.div
    | Expr.MUL -> Mpqf.mul
    | x ->
        Format.printf "cant evaluate binary operator:%a\n" Expr.pp_binop x ;
        failwith "evaluation error"
  in
  let rec eval = function
    | Cst i -> i
    | Neg e -> Q.neg (eval e)
    | Binary (bop, e1, e2) -> (binary bop) (eval e1) (eval e2)
    | Var v -> (
      try param env v |> eval
      with Not_found -> failwith ("cant evaluate variable " ^ v) )
    | _ -> failwith "can't evaluation error"
  in
  eval expr

(* to csp expr conversion *)
let rec to_csp = function
  | Neg (Cst i) -> Expr.of_mpqf (Mpqf.neg i)
  | Neg e -> Expr.Neg (to_csp e)
  | Binary (b, e1, e2) ->
      let e1' = to_csp e1 and e2' = to_csp e2 in
      Expr.Binary (b, e1', e2')
  | Var v -> Expr.var v
  | Cst i -> Expr.of_mpqf i
  | Array (v, i) -> failwith ("can't convert " ^ v ^ "[" ^ string_of_int i ^ "]")

(* to csp expr conversion *)
let rec to_csp_constr = function
  | Cmp (e1, c, e2) -> Constraint.Cmp (to_csp e1, c, to_csp e2)
  | And (b1, b2) -> Constraint.And (to_csp_constr b1, to_csp_constr b2)
  | Or (b1, b2) -> Constraint.Or (to_csp_constr b1, to_csp_constr b2)
  | Not b -> Constraint.Not (to_csp_constr b)

type modstmt =
  | Param of string * expr
  | Var of string * expr * expr
  | ParamList of string * set * expr * expr
  (* p from e1 to e2 *)
  | VarList of string * set * expr * expr
  (* v from e1 to e2 such that e3 <= v <= e4 ex: var x{1..4} >= 0.0, <= 5.0;*)
  | SubjectTo of string * bexpr
  | Ignore

and set = expr * expr

type t = modstmt list

let eval_set env (inf, sup) =
  ( evaluate env inf |> Mpqf.to_float |> int_of_float
  , evaluate env sup |> Mpqf.to_float |> int_of_float )

let toCsp m =
  let add (env, csp) = function
    | Param (v, e) -> (set_param env v e, csp)
    | Var (v, e1, e2) ->
        let e1' = evaluate env e1 and e2' = evaluate env e2 in
        (env, Csp.add_real_var v e1' e2' csp)
    | ParamList (_v, (_lower, _upper), _e1, _e2) ->
        (* let l = evaluate env lower |> int_of_float *)
        (* and u = evaluate env upper |> int_of_float in *)
        (* if l < u then *)
        (* let e = evaluate env e1 and sup = evaluate env e2 in *)
        (* let rec loop i csp = *)
        (* if i > u then csp *)
        (* else *)
        (*       let name = name v i in *)
        (*       (set_param env name e) *)
        (* in *)
        (* (loop l env),csp *)
        (* else failwith ("vars with inf index greater than sup index : "^v) *)
        failwith "param list not implemented for now"
    | VarList (v, set, e1, e2) ->
        let l, u = eval_set env set in
        if l < u then
          let inf = evaluate env e1 and sup = evaluate env e2 in
          let rec loop i csp =
            if i > u then csp
            else
              let name = name v i in
              let csp = Csp.add_real_var name inf sup csp in
              loop (i + 1) csp
          in
          (env, loop l csp)
        else failwith ("vars with inf index greater than sup index : " ^ v)
    | SubjectTo (_, constr) ->
        let constr' = substitute_constr env constr in
        (env, Csp.add_constr csp (to_csp_constr constr'))
    | Ignore -> (env, csp)
  in
  let empty_csp = Csp.empty in
  let empty_env = {params= Env.empty; paramsarray= Env.empty} in
  List.fold_left add (empty_env, empty_csp) m |> snd
