open Tools
open Constraint
open Expr

let is_zero = Q.equal Q.zero

let is_neg c = Q.compare Q.zero c > 0

(** iter on expr *)
let iter_expr f =
  let rec loop = function
    | Binary (_, e1, e2) as b -> f b ; loop e1 ; loop e2
    | Neg e as u -> f u ; loop e
    | x -> f x
  in
  loop

(** iter on constraints *)
let iter_constr f_expr f_constr =
  let rec loop = function
    | Cmp (e1, _, e2) as constr ->
        f_constr constr ; iter_expr f_expr e1 ; iter_expr f_expr e2
    | (And (b1, b2) as constr) | (Or (b1, b2) as constr) ->
        f_constr constr ; loop b1 ; loop b2
    | Not b as constr -> f_constr constr ; loop b
    | constr -> f_constr constr
  in
  loop

(** boolean formulae map *)
let map_constr f =
  let rec loop = function
    | Cmp c -> Cmp (f c)
    | And (b1, b2) -> And (loop b1, loop b2)
    | Or (b1, b2) -> Or (loop b1, loop b2)
    | Not b -> Not (loop b)
    | True -> True
    | False -> False
  in
  loop

let apply f e1 e2 b op =
  let e1', b1 = f e1 b in
  let e2', b2 = f e2 b in
  (Binary (op, e1', e2'), b1 || b2)

let distribute (op, c) =
  let rec loop = function
    | (Funcall _ | Cst _ | Var _) as x -> Binary (op, x, c)
    | Neg e -> Neg (loop e)
    | Binary (POW, _, _) as expr -> Binary (op, expr, c)
    | Binary (b, e, Cst a) when b = op -> Binary (op, e, mul (Cst a) c)
    | Binary (b, Cst a, e) when b = op -> Binary (op, Binary (op, Cst a, c), e)
    | Binary (((DIV | MUL) as b), Cst a, e) ->
        Binary (b, Binary (op, Cst a, c), e)
    | Binary (DIV, e, Cst a) -> Binary (op, e, Binary (DIV, c, Cst a))
    | Binary (MUL, e, Cst a) -> Binary (MUL, e, Binary (op, Cst a, c))
    | Binary (b, e1, e2) -> Binary (b, loop e1, loop e2)
  in
  loop

let rec expand = function
  | (Funcall _ | Cst _ | Var _) as x -> x
  | Neg e -> Neg (expand e)
  | Binary (MUL, Cst c, e) | Binary (MUL, e, Cst c) -> distribute (MUL, Cst c) e
  | Binary (DIV, e, Cst c) -> distribute (DIV, Cst c) e
  | Binary (binop, e1, e2) -> Binary (binop, expand e1, expand e2)

(* simplifies elementary function *)
let rec simplify_expr expr change =
  match expr with
  | Funcall _ | Cst _ | Var _ -> (expr, change)
  | Neg e -> (
    match e with
    | Cst c when is_zero c -> (zero, true)
    | Cst c -> (Cst (Q.neg c), true)
    | Neg e' -> simplify_expr e' true
    | Binary (SUB, Cst a, Cst b) -> (Cst (Q.sub b a), true)
    | Binary (SUB, a1, a2) -> simplify_expr (Binary (SUB, a2, a1)) true
    | _ ->
        let e', b = simplify_expr e change in
        (Neg e', b) )
  | Binary (b, e1, e2) -> (
    match b with
    | ADD -> (
      match (e1, e2) with
      | Cst a, Cst b -> (Cst (Q.add a b), true)
      | Cst z, e2 when is_zero z -> simplify_expr e2 change
      | e1, Cst z when is_zero z -> simplify_expr e1 change
      | (e1, Cst c | Cst c, e1) when is_neg c ->
          simplify_expr (sub e1 (Cst (Q.neg c))) true
      | e1, Neg e2 | Neg e2, e1 -> simplify_expr (sub e1 e2) true
      | e1, e2 -> apply simplify_expr e1 e2 change ADD )
    | SUB -> (
      match (e1, e2) with
      | Cst a, Cst b -> (Cst (Q.sub a b), true)
      | Cst c, _ when is_zero c ->
          let e, _ = simplify_expr e2 change in
          (Neg e, true)
      | _, Cst c when is_zero c -> simplify_expr e1 change
      | e1, Cst c when is_neg c ->
          simplify_expr (Binary (ADD, e1, Cst (Q.neg c))) true
      | Cst c, e1 when is_neg c ->
          simplify_expr (Neg (Binary (ADD, e1, Cst (Q.neg c)))) true
      | _, Neg e -> simplify_expr (Binary (ADD, e1, e)) true
      | Neg e, _ -> simplify_expr (Neg (Binary (ADD, e, e2))) true
      | _, _ -> apply simplify_expr e1 e2 change SUB )
    | MUL -> (
      match (e1, e2) with
      | Cst a, Cst b -> (Cst (Q.mul a b), true)
      | Cst c, _ when is_zero c -> (zero, true)
      | _, Cst c when is_zero c -> (zero, true)
      | Cst c, _ when Q.equal Q.one c -> simplify_expr e2 change
      | _, Cst c when Q.equal Q.one c -> simplify_expr e1 change
      | e1, Cst c when is_neg c ->
          simplify_expr (Neg (Binary (MUL, e1, Cst (Q.neg c)))) true
      | Cst c, e1 when is_neg c ->
          simplify_expr (Neg (Binary (MUL, e1, Cst (Q.neg c)))) true
      | e', Neg e | Neg e, e' -> simplify_expr (Neg (Binary (MUL, e, e'))) true
      | _, _ -> apply simplify_expr e1 e2 change MUL )
    | DIV -> (
      match (e1, e2) with
      | _, Cst c when is_zero c -> (zero, true) (* TODO treat NaN *)
      | Cst c, _ when is_zero c -> (zero, true)
      | _, _ -> apply simplify_expr e1 e2 change DIV )
    | POW -> apply simplify_expr e1 e2 change POW )

(** calls simplify until it reaches a fixpoint *)
let rec simplify_fp expr =
  let e, b = simplify_expr expr false in
  if b then simplify_fp e else e

let rec simplify_bexpr = function
  | Cmp (e1, op, e2) -> Cmp (simplify_fp e1, op, simplify_fp e2)
  | And (b1, b2) -> And (simplify_bexpr b1, simplify_bexpr b2)
  | Or (b1, b2) -> Or (simplify_bexpr b1, simplify_bexpr b2)
  | Not b -> Not (simplify_bexpr b)
  | x -> x

let left_hand_side (e1, op, e2) =
  match (e1, e2) with
  | Cst c, _ when is_zero c -> (inv_cmp op, e2)
  | _, Cst c when is_zero c -> (op, e1)
  | _, _ -> (op, simplify_fp (Binary (SUB, e1, e2)))

let flatten_pow e =
  let rec loop = function
    | 0 -> one
    | 1 -> e
    | n ->
        let sq = loop (n / 2) in
        if n mod 2 = 0 then Binary (MUL, sq, sq)
        else Binary (MUL, e, Binary (MUL, sq, sq))
  in
  loop

(* derives a function regarding a variable *)
let rec derivate expr var =
  match expr with
  | Cst _ -> zero
  | Var v -> if var = v then one else zero
  | Neg e -> Neg (derivate e var)
  | Binary (b, e1, e2) -> (
    match b with
    | ADD -> Binary (ADD, derivate e1 var, derivate e2 var)
    | SUB -> Binary (SUB, derivate e1 var, derivate e2 var)
    | MUL ->
        Binary
          ( ADD
          , Binary (MUL, derivate e1 var, e2)
          , Binary (MUL, e1, derivate e2 var) )
    | DIV ->
        Binary
          ( DIV
          , Binary
              ( SUB
              , Binary (MUL, derivate e1 var, e2)
              , Binary (MUL, e1, derivate e2 var) )
          , square e2 )
    | POW -> (
      match e2 with
      | Cst i -> (
        match Q.to_int i with
        | Some i ->
            let expr' = flatten_pow e1 i in
            derivate expr' var
        | None -> zero )
      | _ -> zero ) )
  | Funcall _ -> zero

(* TODO IMPLEMENTATION *)

let rec derivative var = function
  | Cmp (e1, op, e2) -> Cmp (derivate e1 var, op, derivate e2 var)
  | And (b1, b2) -> And (derivative var b1, derivative var b2)
  | Or (b1, b2) -> Or (derivative var b1, derivative var b2)
  | Not b -> Not (derivative var b)
  | x -> x

let is_arith = function Cmp (_, _, _) -> true | _ -> false

let to_arith = function
  | Cmp c -> c
  | _ -> invalid_arg "not arithmetic constraint"

(*****************************************)
(*        PREPROCESSING FUNCTIONS        *)
(*****************************************)

let get_vars_set_expr e = VarSet.of_list (Expr.collect_vars e |> VarMap.keys)

let get_vars_set_bexpr c =
  VarSet.of_list (Constraint.collect_vars c |> VarMap.keys)
