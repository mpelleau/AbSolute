open Var_store
open Csp
open Abstract_domain
open Bot

module type Box_closure_sig = functor (S: Var_store_sig) ->
sig
  module Store : Var_store_sig
  val incremental_closure: Store.t -> bconstraint -> Store.t
  val entailment: Store.t -> bconstraint -> kleene
end with module Store=S

module Make(Store: Var_store_sig) =
struct
  module Store = Store
  module I = Store.I

  (* The type `Csp.expr` is an AST representing the syntax of an expression.
     In `node`, we annotate each node of this expression with its interval evaluation.
     Note: the function `eval` below transforms `Csp.expr` into `node`. *)
  type node_kind =
    | BFuncall of string * node list
    | BUnary   of unop * node
    | BBinary  of binop * node * node
    | BVar     of var
    | BCst     of I.t
  and node = node_kind * I.t

  (* I. Evaluation part

     First step of the HC4-revise algorithm: it computes the intervals for each node of the expression.
     For example: given `x + 3` with `x in [1..3]`, then it annotates `+` with `[4..6]`.
     It returns this new valued expression tree (`node`), and the interval of the root node.

     This function is also useful for testing transfer functions errors (e.g. division by zero).
     - We raise Bot_found in case the expression only evaluates to error values.
     - Otherwise, we return only the non-error values.
   *)
  let rec eval store = function
  | Funcall(name, args) ->
     let bargs = List.map (eval store) args in
     let iargs = List.map snd bargs in
     let r = debot (I.eval_fun name iargs) in
     BFuncall(name, bargs),r
  | Var v ->
      let r = Store.find v store in
      BVar v, r
  | Cst (c,_) ->
      let r = I.of_rat c in
      BCst r, r
  | Unary (o,e1) ->
      let _,i1 as b1 = eval store e1 in
      let r = match o with
        | NEG -> I.neg i1
      in
      BUnary (o,b1), r
  | Binary (o,e1,e2) ->
     let _,i1 as b1 = eval store e1
     and _,i2 as b2 = eval store e2 in
     let r = match o with
       | ADD -> I.add i1 i2
       | SUB -> I.sub i1 i2
       | DIV -> debot (I.div i1 i2)
       | MUL ->
          let r = I.mul i1 i2 in
          if e1=e2 then
            (* special case: squares are positive *)
            I.abs r
          else r
       | POW -> I.pow i1 i2 in BBinary (o,b1,b2), r

  (* II. Refine part

     Second step of the HC4-revise algorithm.
     It propagates the intervals from the root of the expression tree `e` to the leaves.
     For example: Given `y = x + 3`, `x in [1..3]`, `y in [1..5]`.
                  Then after `eval` we know that the node at `+` has the interval `[4..6]`.
                  Therefore we can intersect `y` with `[4..6]` due to the equality.
     Note that we can call again `eval` to restrain further `+`, and then another round of `refine` will restrain `x` as well.
     We raise `Bot_found` in case of unsatisfiability. *)

  (* refines binary operator to handle constants *)
  let refine_bop f1 f2 (e1,i1) (e2,i2) x (b:bool) =
    match e1, e2, b with
    | BCst c1, BCst c2, _ -> Nb (i1, i2)
    | BCst c, _, true -> merge_bot2 (Nb i1) (f2 i2 i1 x)
    | BCst c, _, false -> merge_bot2 (Nb i1) (f2 i2 x i1)
    | _, BCst c, _ -> merge_bot2 (f1 i1 i2 x) (Nb i2)
    | _, _, true -> merge_bot2 (f1 i1 i2 x) (f2 i2 i1 x)
    | _, _, false -> merge_bot2 (f1 i1 i2 x) (f2 i2 x i1)

  (* u + v = r => u = r - v /\ v = r - u *)
  let refine_add u v r =
    refine_bop I.filter_add_f I.filter_add_f u v r true

  (* u - v = r => u = r + v /\ v = u - r *)
  let refine_sub u v r =
    refine_bop I.filter_sub_f I.filter_add_f u v r false

  (* u * v = r => (u = r/v \/ v=r=0) /\ (v = r/u \/ u=r=0) *)
  let refine_mul u v r =
    refine_bop I.filter_mul_f I.filter_mul_f u v r true

  (* u / v = r => u = r * v /\ (v = u/r \/ u=r=0) *)
  let refine_div u v r =
    refine_bop I.filter_div_f I.filter_mul_f u v r false

  let rec refine store root = function
  | BFuncall(name,args) ->
     let nodes_kind, itv = List.split args in
     let res = I.filter_fun name itv root in
     List.fold_left2 refine store (debot res) nodes_kind
  | BVar v -> Store.add v (debot (I.meet root (Store.find v store))) store
  | BCst i -> ignore (debot (I.meet root i)); store
  | BUnary (o,(e1,i1)) ->
     let j = match o with
       | NEG -> I.filter_neg i1 root
      in refine store (debot j) e1
  | BBinary (o,(e1,i1),(e2,i2)) ->
     let j = match o with
       | ADD -> refine_add (e1,i1) (e2,i2) root
       | SUB -> refine_sub (e1,i1) (e2,i2) root
       | MUL -> refine_mul (e1,i1) (e2,i2) root
       | DIV -> refine_div (e1,i1) (e2,i2) root
       | POW -> I.filter_pow i1 i2 root
     in
     let j1,j2 = debot j in
     refine (refine store j1 e1) j2 e2

  (* III. HC4-revise algorithm (combining eval and refine).

     Apply the evaluation followed by the refine step of the HC4-revise algorithm.
     It prunes the domain of the variables in `store` according to the constraint `e1 o e2`.
  *)
  let hc4_revise store (e1,op,e2) =
    let (b1,i1), (b2,i2) = eval store e1, eval store e2 in
    let j1,j2 = match op with
      | LT  -> debot (I.filter_lt i1 i2)
      | LEQ -> debot (I.filter_leq i1 i2)
      (* a > b <=> b < a*)
      | GEQ -> let j2,j1 = debot (I.filter_leq i2 i1) in (j1,j2)
      | GT  -> let j2,j1 = debot (I.filter_lt i2 i1) in (j1,j2)
      | NEQ -> debot (I.filter_neq i1 i2)
      | EQ  -> debot (I.filter_eq i1 i2)
    in
    let refined_store = if I.equal j1 i1 then store else refine store j1 b1 in
    if j2 = i2 then refined_store else refine refined_store j2 b2

  let incremental_closure store c =
    (* let _ = (Printf.printf "HC4 with %s\n" (string_of_bconstraint c); flush_all ()) in *)
    hc4_revise store c

  let entailment store (e1,op,e2) =
    try
      ignore(hc4_revise store (e1,op,e2));
      try
        ignore(hc4_revise store (e1,neg op,e2));
        Unknown
      with
      | Bot_found -> True
    with
    | Bot_found -> False
end
