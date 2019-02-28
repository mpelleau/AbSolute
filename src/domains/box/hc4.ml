open Var_store
open Csp
open Abstract_domain

module type Box_closure_sig =
sig
  module Store : Var_store_sig
  val closure: Store.t -> bconstraint -> Store.t
  val entailment: Store.t -> bconstraint -> kleene
end

module Make(Store: Var_store_sig) =
struct
  module Store = Store
  let closure box c = box
  let entailment box c = Unknown
end

(*


  (* trees with nodes annotated with evaluation *)
  type bexpr =
    | BFuncall of string * bexpri list
    | BUnary   of unop * bexpri
    | BBinary  of binop * bexpri * bexpri
    | BVar     of var
    | BCst     of i

  and bexpri = bexpr * i

  (* First step of the HC4-revise algorithm: it computes the intervals for each node of the expression.
     For example: given `x + 3` with `x in [1..3]`, then it annotates `+` with `[4..6]`.
     It returns this new annotated expression tree, and the interval of the root node.

     This function is useful for testing transfer functions errors (e.g. division by zero).
     - We raise Bot_found in case the expression only evaluates to error values.
     - Otherwise, we return only the non-error values.
   *)
  let rec eval (a:t) (e:expr) : bexpri =
    match e with
    | Funcall(name,args) ->
       let bargs = List.map (eval a) args in
       let iargs = List.map snd bargs in
       let r = debot (I.eval_fun name iargs) in
       BFuncall(name, bargs),r
    | Var v ->
        let r = find v a in
        BVar v, r
    | Cst (c,_) ->
        let r = I.of_rat c in
        BCst r, r
    | Unary (o,e1) ->
        let _,i1 as b1 = eval a e1 in
        let r = match o with
          | NEG -> I.neg i1
        in
        BUnary (o,b1), r
    | Binary (o,e1,e2) ->
       let _,i1 as b1 = eval a e1
       and _,i2 as b2 = eval a e2 in
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

  u / v = r => u = r * v /\ (v = u/r \/ u=r=0)
  let refine_div u v r =
    refine_bop I.filter_div_f I.filter_mul_f u v r false

  (* Second step of the HC4-revise algorithm.
     It propagates the intervals from the root of the expression tree `e` to the leaves.
     For example: Given `y = x + 3`, `x in [1..3]`, `y in [1..5]`.
                  Then after `eval` we know that the node at `+` has the interval `[4..6]`.
                  Therefore we can intersect `y` with `[4..6]` due to the equality.
     Note that we can call again `eval` to restrain further `+`, and then another round of `refine` will restrain `x` as well.
     We raise `Bot_found` in case of unsatisfiability. *)
  let rec refine (a:t) (e:bexpr) (x:i) : t =
    (*Format.printf "%a\n" print_bexpri (e, x);*)
    match e with
    | BFuncall(name,args) ->
       let bexpr,itv = List.split args in
       let res = I.filter_fun name itv x in
       List.fold_left2 (fun acc e1 e2 ->
           refine acc e2 e1) a (debot res) bexpr
    | BVar v -> Env.add v (debot (I.meet x (find v a))) a
    | BCst i -> ignore (debot (I.meet x i)); a
    | BUnary (o,(e1,i1)) ->
       let j = match o with
         | NEG -> I.filter_neg i1 x
        in refine a e1 (debot j)
    | BBinary (o,(e1,i1),(e2,i2)) ->
       let j = match o with
         | ADD -> refine_add (e1,i1) (e2,i2) x
         | SUB -> refine_sub (e1,i1) (e2,i2) x
         | MUL -> refine_mul (e1,i1) (e2,i2) x
         | DIV -> refine_div (e1,i1) (e2,i2) x
         | POW -> I.filter_pow i1 i2 x
       in
       let j1,j2 = debot j in
       refine (refine a e1 j1) e2 j2

  (* test transfer function.
     Apply the evaluation followed by the refine step of the HC4-revise algorithm.
     It prunes the domain of the variables in `a` according to the constraint `e1 o e2`.
  *)
  let test (a:t) (e1:expr) (o:cmpop) (e2:expr) : t bot =
    Tools.debug 2 "HC4 - eval\n%!";
    let (b1,i1), (b2,i2) = eval a e1, eval a e2 in
    (*Format.printf "%a %a %a\n" print_bexpri (b1, i1) print_cmpop o print_bexpri (b2, i2);*)
    let j1,j2 = match o with
      | LT  -> debot (I.filter_lt i1 i2)
      | LEQ -> debot (I.filter_leq i1 i2)
      (* a > b <=> b < a*)
      | GEQ -> let j2,j1 = debot (I.filter_leq i2 i1) in (j1,j2)
      | GT  -> let j2,j1 = debot (I.filter_lt i2 i1) in (j1,j2)
      | NEQ -> debot (I.filter_neq i1 i2)
      | EQ  -> debot (I.filter_eq i1 i2)
    in
    Tools.debug 2 "HC4 - refine\n%!";
    let refined1 = if I.equal j1 i1 then a else refine a b1 j1 in
    Nb(if j2 = i2 then refined1 else refine refined1 b2 j2)

  let filter (a:t) (e1,binop,e2) : t =
    match test a e1 binop e2 with
    | Bot ->
       Tools.debug 5 "\n%a\n\t%a\n" print_bexpr (Cmp(binop, e1, e2)) print a;
       raise Bot_found
    | Nb e -> e *)