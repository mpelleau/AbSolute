open Csp
open Tools

(** {1 Constants }*)
let one = Cst Q.one
let zero = Cst Q.zero
let two  = Cst Q.two

(** {1 Expression Constructors} *)

(** builds an expression from an integer *)
let of_int n = Cst (Q.of_int n)

(** builds an expression from an float *)
let of_float f = Cst (Q.of_float f)

(** builds an expression from an Mpqf.t *)
let of_mpqf m = Cst m

(** given an expression [e] builds the expresspion for [e*e]*)
let square expr = Binary (POW, expr, two)

(** {1 Constraint constructor} *)

(** {2 comparisons} *)
let leq e1 e2 = Cmp (e1,LEQ,e2)
let lt  e1 e2 = Cmp (e1,LT,e2)
let geq e1 e2 = Cmp (e1,GEQ,e2)
let gt  e1 e2 = Cmp (e1,GT,e2)
let eq  e1 e2 = Cmp (e1,EQ,e2)
let neq e1 e2 = Cmp (e1,NEQ,e2)

(** constraint for variable assignment by a constant *)
let assign var value =
  eq (Var var) (of_mpqf value)

(** constraint for 'v \in [low;high]' *)
let inside v low high =
  let v = Var v in
  And ((geq v (of_mpqf low)), (leq v (of_mpqf high)))

(** {1 Predicates} *)

(** checks if an expression contains a variable *)
let rec has_variable = function
  | Funcall(_,args) -> List.exists has_variable args
  | Neg e -> has_variable e
  | Binary(_, e1, e2) -> has_variable e1 || has_variable e2
  | Var _ -> true
  | Cst _ -> false

(** checks if an expression is linear *)
let rec is_linear = function
  | Neg e -> is_linear e
  | Binary(MUL, e1, e2) | Binary(DIV, e1, e2)
    -> not (has_variable e1 && has_variable e2) && is_linear e1 && is_linear e2
  | Binary(POW, e1, e2)
    -> not (has_variable e1 || has_variable e2)
  | Binary(_, e1, e2) -> is_linear e1 && is_linear e2
  | Var _ | Cst _ -> true
  | _ -> false

let is_zero = Q.equal Q.zero
let is_neg c = Q.compare Q.zero c > 0

(** {1 Operations} *)

(** cmp operator inversion *)
let inv = function
  | EQ  -> EQ
  | LEQ -> GEQ
  | GEQ -> LEQ
  | NEQ -> NEQ
  | GT  -> LT
  | LT  -> GT

(** comparison operator negation *)
let neg = function
  | EQ  -> NEQ
  | LEQ -> GT
  | GEQ -> LT
  | NEQ -> EQ
  | GT  -> LEQ
  | LT  -> GEQ

(** converts a domain representation to a constraint *)
(* TODO: use type *)
let domain_to_constraints ((_,v,d):decl) : bexpr =
  match d with
  | Finite (l,h) -> inside v l h
  | Minf i -> leq (Var v) (Cst i)
  | Inf i -> geq (Var v) (Cst i)
  | Set (h::tl) ->
     List.fold_left (fun acc e ->
         Or(acc, assign v e)
       ) (assign v h) tl
  | _ -> eq one one

(** iter on expr *)
let rec iter_expr f = function
  | Binary (_,e1,e2) as b -> f b; iter_expr f e1; iter_expr f e2
  | Neg e as u -> f u; iter_expr f e
  | x -> f x

(** iter on constraints *)
let rec iter_constr f_expr f_constr = function
  | Cmp (e1,_,e2) as constr ->
     f_constr constr;
     iter_expr f_expr e1;
     iter_expr f_expr e2
  | (And (b1,b2) as constr)
  | (Or  (b1,b2) as constr) ->
     f_constr constr;
     iter_constr f_expr f_constr b1;
     iter_constr f_expr f_constr b2
  | Not b as constr ->
     f_constr constr;
     iter_constr f_expr f_constr b

(** boolean formulae map *)
let map_constr f =
  let rec loop = function
    | Cmp c -> Cmp (f c)
    | And (b1,b2) -> And (loop b1, loop b2)
    | Or (b1,b2) -> Or (loop b1, loop b2)
    | Not b -> Not (loop b)
  in loop

(** constraint negation *)
let rec neg_bexpr = function
  | Cmp (e1,op,e2) -> Cmp(e1,neg op,e2)
  | And (b1,b2) -> Or (neg_bexpr b1, neg_bexpr b2)
  | Or (b1,b2) -> And (neg_bexpr b1, neg_bexpr b2)
  | Not b -> b

(** rewrites a constraint into an equivalent constraint without 'Not' *)
let rec remove_not = function
  | Not b -> remove_not (neg_bexpr b)
  | And (b1,b2) -> And(remove_not b1, remove_not b2)
  | Or (b1,b2) -> Or(remove_not b1, remove_not b2)
  | x -> x

let apply f e1 e2 b op =
  let (e1', b1) = f e1 b in
  let (e2', b2) = f e2 b in
  (Binary (op, e1', e2'), b1 || b2)

let rec distribute ((op,c) as opc) = function
  | (Funcall _ | Cst _ | Var _) as x  ->  Binary (op, x, c)
  | Neg e -> Neg (distribute opc e)
  | Binary (POW, _, _) as expr -> Binary (op, expr, c)
  | Binary (b, e, Cst a) when b = op -> Binary (op, e, Binary (MUL, Cst a, c))
  | Binary (b, Cst a, e) when b = op -> Binary (op, Binary (op, Cst a, c), e)
  | Binary (DIV|MUL as b, Cst a, e) -> Binary (b, Binary (op, Cst a, c), e)
  | Binary (DIV, e, Cst a) -> Binary (op, e, Binary (DIV, c, Cst a))
  | Binary (MUL, e, Cst a) -> Binary (MUL, e, Binary (op, Cst a, c))
  | Binary (b, e1, e2) -> Binary (b, distribute opc e1, distribute opc e2)

let rec expand = function
  | (Funcall _ | Cst _ | Var _) as x  -> x
  | Neg e -> Neg (expand e)
  | Binary (MUL, Cst c, e) | Binary (MUL, e, Cst c) -> distribute (MUL, Cst c) e
  | Binary (DIV, e, Cst c) -> distribute (DIV, Cst c) e
  | Binary (binop, e1, e2) -> Binary (binop, expand e1, expand e2)

(* simplifies elementary function *)
let rec simplify_expr expr change =
  match expr with
  | Funcall _ | Cst _ | Var _ -> expr,change
  | Neg e ->
    (match e with
     | Cst c when is_zero c -> (zero, true)
     | Cst c -> (Cst (Q.neg c), true)
     | Neg e' -> simplify_expr e' true
     | Binary (SUB, Cst a, Cst b) -> (Cst (Mpqf.sub b a), true)
     | Binary (SUB, a1, a2) -> simplify_expr (Binary (SUB, a2, a1)) true
     | _ -> let (e', b) = simplify_expr e change in (Neg e'), b
    )
  | Binary (b, e1, e2) ->
    (match b with
     | ADD ->
       (match e1, e2 with
        | Cst a, Cst b -> (Cst (Mpqf.add a b), true)
        | Cst z, e2 when is_zero z -> simplify_expr e2 change
        | e1, Cst z when is_zero z -> simplify_expr e1 change
        | e1 , Cst c when is_neg c ->
           simplify_expr (Binary(SUB, e1, Cst (Mpqf.neg c))) true
        | Cst c, e1 when is_neg c ->
           simplify_expr (Binary(SUB, e1, Cst (Mpqf.neg c))) true
        | e1, Neg e -> simplify_expr (Binary(SUB, e1, e)) true
        | Neg e, e2 -> simplify_expr (Binary(SUB, e2, e)) true
        | e1, e2 -> apply simplify_expr e1 e2 change ADD
       )
     | SUB ->
       (match e1, e2 with
        | Cst a, Cst b -> (Cst ((Mpqf.sub a b)), true)
        | Cst c, _ when is_zero c->
           let (e, _) = simplify_expr e2 change in (Neg e, true)
        | _, Cst c when is_zero c -> simplify_expr e1 change
        | e1 , Cst c when is_neg c -> simplify_expr (Binary(ADD, e1, Cst (Q.neg c))) true
        | Cst c, e1 when is_neg c -> simplify_expr (Neg (Binary(ADD, e1, Cst (Q.neg c)))) true
        | _, Neg e -> simplify_expr (Binary(ADD, e1, e)) true
        | Neg e, _ -> simplify_expr (Neg (Binary(ADD, e, e2))) true
        | _, _ -> apply simplify_expr e1 e2 change SUB
       )
     | MUL ->
       (match e1, e2 with
        | Cst a, Cst b -> (Cst (Mpqf.mul a b), true)
        | Cst c, _ when is_zero c -> (zero, true)
        | _, Cst c when is_zero c -> (zero, true)
        | Cst c, _ when Mpqf.equal Q.one c -> simplify_expr e2 change
        | _, Cst c when Mpqf.equal Q.one c -> simplify_expr e1 change
        | e1 , Cst c when is_neg c -> simplify_expr ((Neg (Binary(MUL, e1, Cst (Mpqf.neg c))))) true
        | Cst c, e1 when is_neg c -> simplify_expr (Neg (Binary(MUL, e1, Cst (Mpqf.neg c)))) true
        | e', Neg e | Neg e, e' -> simplify_expr ((Neg (Binary(MUL, e, e')))) true
        | _, _ -> apply simplify_expr e1 e2 change MUL
       )
     | DIV ->
       (match e1, e2 with
        | _, Cst c when is_zero c -> (zero, true) (* TODO treat NaN *)
        | Cst c, _ when is_zero c -> (zero, true)
        | _, _ -> apply simplify_expr e1 e2 change DIV
       )
     | POW -> apply simplify_expr e1 e2 change POW

    )

(** calls simplify until it reaches a fixpoint *)
let rec simplify_fp expr =
  let (e, b) = simplify_expr expr false in
  if b then simplify_fp e
  else e

let rec simplify_bexpr = function
  | Cmp (e1,op,e2) -> Cmp (simplify_fp e1,op, simplify_fp e2)
  | And (b1,b2) -> And (simplify_bexpr b1, simplify_bexpr b2)
  | Or (b1,b2) -> Or (simplify_bexpr b1, simplify_bexpr b2)
  | Not b -> Not (simplify_bexpr b)

let left_hand_side (e1,op,e2) =
  match e1, e2 with
  | Cst c, _  when is_zero c-> (inv op, e2)
  | _, Cst c when is_zero c -> (op, e1)
  | _, _ -> (op, simplify_fp (Binary (SUB, e1, e2)))

let rec left_hand = function
  | Cmp (e1,op,e2) -> left_hand_side (e1,op,e2)
  | And (b1,_) | Or (b1,_) -> left_hand b1
  | Not b -> left_hand b

let flatten_pow e =
  let rec loop = function
  | 0 -> one
  | 1 -> e
  | n -> let sq = loop (n/2) in
         if n mod 2 = 0 then Binary (MUL, sq, sq)
         else Binary (MUL, e,Binary (MUL, sq, sq))
  in loop

(* derives a function regarding a variable *)
let rec derivate expr var =
  match expr with
  | Cst _ -> zero
  | Var v -> if var = v then one else zero
  | Neg e ->  Neg (derivate e var)
  | Binary (b, e1, e2) ->
    (match b with
     | ADD -> Binary (ADD, derivate e1 var, derivate e2 var)
     | SUB -> Binary (SUB, derivate e1 var, derivate e2 var)
     | MUL -> Binary (ADD, Binary (MUL, derivate e1 var, e2), Binary (MUL, e1, derivate e2 var))
     | DIV -> Binary (DIV, Binary (SUB, Binary (MUL, derivate e1 var, e2), Binary (MUL, e1, derivate e2 var)), square e2)
     | POW -> begin
        match e2 with
        | Cst i -> begin
            match Q.to_int i with
            | Some i -> let expr' = flatten_pow e1 i in
                derivate expr' var
            | None -> zero
            end
        | _ -> zero
        end
    )
  | Funcall _ -> zero (* TODO IMPLEMENTATION *)

let rec derivative bexpr var =
  match bexpr with
  | Cmp (e1,op,e2) -> Cmp (derivate e1 var,op, derivate e2 var)
  | And (b1,b2) -> And (derivative b1 var, derivative b2 var)
  | Or (b1,b2) -> Or (derivative b1 var, derivative b2 var)
  | Not b -> Not (derivative b var)

let is_arith = function
  | Cmp (_, _, _) -> true
  | _ -> false

let ctr_jacobian c vars =
  List.fold_left (
    fun l (_, v, _) ->
      let expr = if is_arith c then
          let new_c = simplify_bexpr (derivative c v) in
          let (_, e) = left_hand new_c in
          e
        else zero
      in
    (v, expr)::l
  ) [] vars

let compute_jacobian csp =
  List.fold_left (
    fun l c -> (c, ctr_jacobian c csp.init)::l
  ) [] csp.constraints


(*****************************************)
(*        USEFUL FUNCTION ON AST         *)
(*****************************************)

let empty = {init = []; constraints= []; objective = zero; solutions = None}

let get_vars p =
  List.map (fun (_,v,_) -> v) p.init

let add_real_var csp name inf sup =
  let assign = (Real, name, (Finite(inf,sup))) in
  {csp with init = assign::csp.init}

let add_constr csp c =
  {csp with constraints = c::csp.constraints}

(*****************************************)
(*        PREPROCESSING FUNCTIONS        *)
(*****************************************)

let rec replace_cst_expr (id, cst) expr =
  match expr with
  | Var v when v = id -> Cst cst
  | Neg e -> Neg (replace_cst_expr (id, cst) e)
  | Binary (op, e1, e2) -> Binary (op, replace_cst_expr (id, cst) e1, replace_cst_expr (id, cst) e2)
  | Funcall (v, e) -> Funcall (v, List.map(replace_cst_expr (id, cst))e)
  | _ as e -> e

let rec replace_cst_bexpr cst = function
  | Cmp (e1,op, e2) -> Cmp (replace_cst_expr cst e1,op, replace_cst_expr cst e2)
  | And (b1, b2) -> And (replace_cst_bexpr cst b1, replace_cst_bexpr cst b2)
  | Or (b1, b2) -> Or (replace_cst_bexpr cst b1, replace_cst_bexpr cst b2)
  | Not b -> Not (replace_cst_bexpr cst b)

let rec get_vars_expr = function
  | Cst _              -> []
  | Var v              -> [v]
  | Neg e       -> get_vars_expr e
  | Binary (_, e1, e2) -> List.rev_append (get_vars_expr e1) (get_vars_expr e2)
  | Funcall (_,args)   -> List.concat (List.map get_vars_expr args)

let get_vars_set_expr expr = VarSet.of_list (get_vars_expr expr)

let rec get_vars_bexpr = function
  | Cmp (e1,_, e2) -> List.append (get_vars_expr e1) (get_vars_expr e2)
  | And (b1, b2) -> List.append (get_vars_bexpr b1) (get_vars_bexpr b2)
  | Or (b1, b2) -> List.append (get_vars_bexpr b1) (get_vars_bexpr b2)
  | Not b -> get_vars_bexpr b

let get_vars_set_bexpr bexpr = VarSet.of_list (get_vars_bexpr bexpr)

let get_vars_jacob jacob =
  List.map (fun (c, j) -> (c, get_vars_set_bexpr c, j)) jacob

let replace_cst_jacob (id, cst) jacob =
  List.map (fun (c, vars, j) ->
    if VarSet.mem id vars then
      (replace_cst_bexpr (id, cst) c, VarSet.remove id vars, j)
    else (c, vars, j)
  ) jacob

let from_cst_to_expr (id, (l, u)) =
  if l = u then [(Var id, EQ, Cst l)]
  else [(Var id, GEQ, Cst l); (Var id, LEQ, Cst u)]

let csts_to_expr csts =
  List.fold_left (fun l cst -> List.append (from_cst_to_expr cst) l) [] csts
