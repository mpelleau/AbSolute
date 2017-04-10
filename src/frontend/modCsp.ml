open Csp

(* numeric expressions *)
type expr =
  | Unary of unop * expr
  | Binary of binop * expr * expr
  | Var of var
  | Array of var * int
  | Cst of i

(* boolean expressions *)
type bexpr =
  | Cmp of cmpop * expr * expr
  | And of bexpr * bexpr
  | Or of bexpr * bexpr
  | Not of bexpr

module Env = Map.Make(struct type t = var let compare = compare end)
module Vars = Set.Make(struct type t = var let compare = compare end)

type env = {
    vars : Vars.t;
    params : expr Env.t;
    paramsarray : expr array Env.t
  }

let set_param env p e = {env with params = Env.add p e env.params}

let param env p = Env.find p env.params

let name v i = v^"_"^(string_of_int i)

(*set the variable name associated to v[i] -> v_i*)
let set_var_i env v i =
  if Vars.mem (name v i) env.vars then
    failwith ""
  else
    {env with vars = Vars.add (name v i) env.vars}

let param_i env p i =
  let arr = Env.find p env.paramsarray in
  arr.(i)

(*returns the variable name associated to v[i] -> v_i*)
let get_var_i env v i =
  let name = name v i in
  try Vars.find name env.vars
  with Not_found -> failwith ("can't find variable "^name)

let get env name i =
  try param_i env name i
  with Not_found ->
       try Var (get_var_i env name i)
       with Not_found -> failwith ("cantfind array "^name)

(*replaces the param by the expression they represent*)
let rec substitute env = function
  | Unary (NEG, Cst i) -> Cst (-.i)
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

let rec substitute_constr env =
  let rec subs = function
  | Cmp (c,e1,e2) -> Cmp(c, (substitute env e1), (substitute env e2))
  | And (b1,b2) -> And (subs b1, subs b2)
  | Or  (b1,b2) -> Or (subs b1, subs b2)
  | Not b -> Not (subs b)
  in subs

(* force evaluation of some expressions *)
let evaluate env expr =
  let unary = function
    | NEG -> fun x -> (-. x)
    | x -> Format.printf "cant evaluate unary operator:%a\n" print_unop x;
           failwith "evaluation error"
  in
  let binary = function
    | ADD -> (+.)
    | SUB -> (-.)
    | DIV -> (/.)
    | MUL -> ( *. )
    | x -> Format.printf "cant evaluate binary operator:%a\n" print_binop x;
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
  | Unary (NEG, Cst i)-> Csp.Cst (-.i)
  | Unary (u, e)-> Csp.Unary (u,to_csp e)
  | Binary (b, e1, e2)->
     let e1' = to_csp e1
     and e2' = to_csp e2
     in Csp.Binary (b,e1',e2')
  | Var v -> Csp.Var v
  | Cst i -> Csp.Cst i
  | Array (v,i) -> failwith ("can't convert "^v^"["^(string_of_int i)^"]")

(* to csp expr conversion *)
let rec to_csp_constr = function
  | Cmp (c,e1,e2) -> Csp.Cmp(c, (to_csp e1), (to_csp e2))
  | And (b1,b2) -> Csp.And (to_csp_constr b1, to_csp_constr b2)
  | Or  (b1,b2) -> Csp.Or (to_csp_constr b1, to_csp_constr b2)
  | Not b -> Csp.Not (to_csp_constr b)

type modstmt =
  | Param     of var * expr
  | Var       of var * expr * expr
  | ParamList of var * expr * expr
  (* p from e1 to e2 *)
  | VarList   of var * expr * expr * expr * expr
  (* v from e1 to e2 such that e3 <= v <= e4 ex:
     var x{1..4} >= 0.0, <= 5.0;*)
  | SubjectTo of var * bexpr

type t = modstmt list

let toCsp m =
  let add (env,csp) = function
   | Param (v, e) ->
      (set_param env v e),csp

   | Var (v, e1, e2) ->
      let e1' = evaluate env e1 and e2' = evaluate env e2 in
      (env,add_real_var csp v e1' e2')

   | ParamList (v, e1, e2) ->
      failwith "cant handle paramlist for now"

   | VarList  (v, lower, upper, e1, e2) ->
       let l = evaluate env lower |> int_of_float
       and u = evaluate env upper |> int_of_float in
       if l < u then
         let inf = evaluate env e1 and sup = evaluate env e2 in
         let rec loop i csp =
           if i = u then csp
           else
             let name = name v i in
             let csp = add_real_var csp name inf sup in
             loop (i+1) csp
         in
         env,(loop l csp)
       else failwith ("vars with inf index greater than sup index : "^v)
   | SubjectTo (v, constr) ->
      let constr' = substitute_constr env constr in
      env,(add_constr csp (to_csp_constr constr'))
  in
  let empty_csp =
    Csp.({init = [];
          constraints= [];
          objective =Cst(0.);
          to_draw=[]})
  in
  let empty_env = {vars=Vars.empty;
                   params=Env.empty;
                   paramsarray=Env.empty}
  in
  List.fold_left add (empty_env,empty_csp) m |> snd
