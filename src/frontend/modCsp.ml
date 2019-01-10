open Tools

(* numeric expressions *)
type expr =
  | Unary of Csp.unop * expr
  | Binary of Csp.binop * expr * expr
  | Var of Csp.var
  | Array of Csp.var * int
  | Cst of Csp.i

(* boolean expressions *)
type bexpr =
  | Cmp of Csp.cmpop * expr * expr
  | And of bexpr * bexpr
  | Or of bexpr * bexpr
  | Not of bexpr

module Env = VarMap

type env = {
    params      : expr Env.t;
    paramsarray : expr array Env.t
  }

let set_param env p e = {env with params = Env.add p e env.params}

let param env p = Env.find p env.params

let name v i = v^(string_of_int i)

let param_i env p i =
  let arr = Env.find p env.paramsarray in
  arr.(i)

let get env name i =
  try param_i env name i
  with Not_found -> failwith ("cantfind array "^name)

(*replaces the param by the expression they represent*)
let rec substitute env = function
  | Unary (Csp.NEG, Cst i) -> Cst (Bound_rat.neg i)
  | Unary (u, e)-> Unary (u,substitute env e)
  | Binary (b, e1, e2)->
     let e1' = substitute env e1
     and e2' = substitute env e2
     in Binary (b,e1',e2')
  | Var v ->
     (try param env v |> substitute env
      with Not_found -> Var v)
  | Array (v,i) ->
     ( try param_i env v i |> substitute env
       with Not_found -> Var (name v i))
  | Cst i -> Cst i

let substitute_constr env =
  let rec subs = function
  | Cmp (c,e1,e2) -> Cmp(c, (substitute env e1), (substitute env e2))
  | And (b1,b2) -> And (subs b1, subs b2)
  | Or  (b1,b2) -> Or (subs b1, subs b2)
  | Not b -> Not (subs b)
  in subs

(* force evaluation of some expressions *)
let evaluate env expr =
  let unary = function Csp.NEG -> fun x -> (Bound_rat.neg x) in
  let binary = function
    | Csp.ADD -> (Bound_rat.add)
    | Csp.SUB -> (Bound_rat.sub)
    | Csp.DIV -> (Bound_rat.div)
    | Csp.MUL -> (Bound_rat.mul)
    | x -> Format.printf "cant evaluate binary operator:%a\n" Csp.print_binop x;
           failwith "evaluation error"
  in
  let rec eval = function
    | Cst i -> i
    | Unary (uop, e) -> (unary uop) (eval e)
    | Binary (bop, e1, e2) -> (binary bop) (eval e1) (eval e2)
    | Var v ->
       (try param env v |> eval
      with Not_found -> failwith ("cant evaluate variable "^v))
    | _ -> failwith "can't evaluation error"
  in
  eval expr

(* to csp expr conversion *)
let rec to_csp = function
  | Unary (Csp.NEG, Cst i)-> Csp.Cst (Bound_rat.neg i,Csp.Real)
  | Unary (u, e)-> Csp.Unary (u,to_csp e)
  | Binary (b, e1, e2)->
     let e1' = to_csp e1
     and e2' = to_csp e2
     in Csp.Binary (b,e1',e2')
  | Var v -> Csp.Var v
  | Cst i -> Csp.Cst (i,Csp.Real)
  | Array (v,i) -> failwith ("can't convert "^v^"["^(string_of_int i)^"]")

(* to csp expr conversion *)
let rec to_csp_constr = function
  | Cmp (c,e1,e2) -> Csp.Cmp(c, (to_csp e1), (to_csp e2))
  | And (b1,b2) -> Csp.And (to_csp_constr b1, to_csp_constr b2)
  | Or  (b1,b2) -> Csp.Or (to_csp_constr b1, to_csp_constr b2)
  | Not b -> Csp.Not (to_csp_constr b)

type modstmt =
  | Param     of Csp.var * expr
  | Var       of Csp.var * expr * expr
  | ParamList of Csp.var * set * expr * expr
  (* p from e1 to e2 *)
  | VarList   of Csp.var * set * expr * expr
  (* v from e1 to e2 such that e3 <= v <= e4 ex:
     var x{1..4} >= 0.0, <= 5.0;*)
  | SubjectTo of Csp.var * bexpr
  | Ignore

and set = expr * expr

type t = modstmt list

let eval_set env (inf,sup) =
  (evaluate env inf |> Bound_rat.to_float_up |> int_of_float),
  (evaluate env sup |> Bound_rat.to_float_up |> int_of_float)

let toCsp m =
  let add (env,csp) = function
   | Param (v, e) ->
      (set_param env v e),csp

   | Var (v, e1, e2) ->
      let e1' = evaluate env e1 and e2' = evaluate env e2 in
      (env,Csp.add_real_var csp v e1' e2')

   | ParamList (v, (lower, upper), e1, e2) ->
       (* let l = evaluate env lower |> int_of_float *)
       (* and u = evaluate env upper |> int_of_float in *)
       (* if l < u then *)
       (*   let e = evaluate env e1 and sup = evaluate env e2 in *)
       (*   let rec loop i csp = *)
       (*     if i > u then csp *)
       (*     else *)
       (*       let name = name v i in *)
       (*       (set_param env name e) *)
       (*   in *)
       (*   (loop l env),csp *)
       (* else failwith ("vars with inf index greater than sup index : "^v) *)
      failwith "param list not implemented for now"
   | VarList  (v, set, e1, e2) ->
       let l,u = eval_set env set in
       if l < u then
         let inf = evaluate env e1 and sup = evaluate env e2 in
         let rec loop i csp =
           if i > u then csp
           else
             let name = name v i in
             let csp = Csp.add_real_var csp name inf sup in
             loop (i+1) csp
         in
         env,(loop l csp)
       else failwith ("vars with inf index greater than sup index : "^v)

   | SubjectTo (v, constr) ->
      let constr' = substitute_constr env constr in
      env,(Csp.add_constr csp (to_csp_constr constr'))
   | Ignore -> env,csp
  in
  let empty_csp = Csp.empty in
  let empty_env = {params=Env.empty; paramsarray=Env.empty} in
  List.fold_left add (empty_env,empty_csp) m |> snd
