(* variables are identified by a string *)
type var = string

(* constants are floats (the domain of the variable *)
type i = float

(* unary arithmetic operators *)
type unop = NEG | SQRT | ABS | COS | SIN | TAN | COT
	  | ASIN | ACOS | ATAN | ACOT | LN | LOG | EXP

(* binary arithmetic operators *)
type binop = ADD | SUB | MUL | DIV | POW | NROOT | MIN | MAX

(* arithmetic comparison operators *)
type cmpop =
  | EQ | LEQ | GEQ | NEQ | GT | LT

(* numeric expressions *)
type expr =
  | Unary of unop * expr
  | Binary of binop * expr * expr
  | Var of var
  | Cst of i

(* boolean expressions *)
type bexpr =
  | Cmp of cmpop * expr * expr
  | And of bexpr * bexpr
  | Or of bexpr * bexpr
  | Not of bexpr

type typ = INT | REAL

type dom = Finite of i*i | Minf of i | Inf of i | Top

(* assign *)
type assign = (typ * var * dom)

(* declarations *)
type decls =  assign list

(* statements *)
type constrs = bexpr list

(* jacobian *)
type jacob = (var * expr) list

type ctrs = (bexpr * jacob) list

(* constants *)
type csts = (var * (i*i)) list

(* program *)
type prog = { init : decls; constants : csts; objective : expr; constraints : constrs; jacobian : ctrs; to_draw : var list; view : jacob}


(*************************************************************)
(*                         PREDICATES                        *)
(*************************************************************)

(* checks if an expression contains a variable *)
let rec has_variable = function
  | Unary (u, e) -> has_variable e
  | Binary(b, e1, e2) -> has_variable e1 || has_variable e2
  | Var _ -> true
  | Cst _ -> false

(* checks if an expression is linear *)
let rec is_linear = function
  | Unary (NEG,e) -> is_linear e
  | Binary(MUL, e1, e2) | Binary(DIV, e1, e2)
    -> not (has_variable e1 && has_variable e2) && is_linear e1 && is_linear e2
  | Binary(POW, e1, e2)
    -> not (has_variable e1 || has_variable e2)
  | Binary(_, e1, e2) -> is_linear e1 && is_linear e2
  | Var _ | Cst _ -> true
  | _ -> false

(* checks if a constraints is linear *)
let rec is_cons_linear = function
  | Cmp (_,e1,e2) -> is_linear e1 && is_linear e2
  | And (b1,b2) -> is_cons_linear b1 && is_cons_linear b2
  | Or (b1,b2) -> is_cons_linear b1 && is_cons_linear b2
  | Not b -> is_cons_linear b



(*****************************************)
(*        SYMBOLIC TRANSFORMATION        *)
(*****************************************)

(* cmp operator inversion *)
let inv = function
  | EQ -> EQ
  | LEQ -> GEQ
  | GEQ -> LEQ
  | NEQ -> NEQ
  | GT -> LT
  | LT -> GT


let one = Cst 1.
let zero = Cst 0.
let sqr expr = Binary (POW, expr, Cst 2.)
let plus_one expr = Binary (ADD, one, expr)

let apply f e1 e2 b op =
  let (e1', b1) = f e1 b in
  let (e2', b2) = f e2 b in
  (Binary (op, e1', e2'), b1 || b2)

let is_cst = function
  | Cst _ -> true
  | _ -> false

let rec to_cst c b = function
  | [] -> Cst c
  | (Cst a)::t -> if b then to_cst (c +. a) b t else to_cst (c *. a) b t
  | h::t -> to_cst c b t

let rec to_fcst c = function
  | [] -> c
  | (Cst a)::t -> to_fcst (c +. a) t
  | h::t -> to_fcst c t

let rec equal_expr e1 e2 =
  match (e1, e2) with
  | (Cst a, Cst b) -> a = b
  | (Var v1, Var v2) -> v1 = v2
  | (Unary (op1, e1), Unary (op2, e2)) -> op1 = op2 && equal_expr e1 e2
  | (Binary (op1, a, b), Binary (op2, c, d)) -> op1 = op2 && (((equal_expr a c) && (equal_expr b d)) || ((equal_expr a d) && (equal_expr b c)))
  | _ -> false

let simp_expr e1 e2 =
  if equal_expr e1 e2 then
    1
  else
    match e2 with
    | Unary (NEG, e) -> if equal_expr e1 e then -1 else 0
    | _ -> 0

let rec simp_lexpr e n rest = function
  | [] -> (e, n, rest)
  | h::t ->
      let d = simp_expr e h in
      if d = 0 then
        simp_lexpr e n (h::rest) t
      else
        simp_lexpr e (n + d) rest t

let rec simplify_lexpr newl = function
  | [] -> newl
  | h::t ->
      let (_, n, r) = simp_lexpr h 1 [] t in
      match n with
      | 1 -> simplify_lexpr (h::newl) r
      | _ -> simplify_lexpr ((Binary(MUL, Cst (float n), h))::newl) r

let rec simplify_qexpr newl = function
  | [] -> newl
  | h::t ->
      let (_, n, r) = simp_lexpr h 1 [] t in
      match n with
      | 1 -> simplify_qexpr (h::newl) r
      | _ -> simplify_qexpr ((Binary(POW, h, Cst (float n)))::newl) r

let rec expr_from_list op neutre = function
  | [] -> neutre
  | [s] -> s
  | h::t -> Binary(op, h, (expr_from_list op neutre t))

let get_lexpr l f op neutre =
  let (c, e) = List.partition is_cst l in
  let cst = if op = ADD then to_cst 0. true c else to_cst 1. false c in
  let le = f [] e in
  if cst = neutre then expr_from_list op neutre le
  else expr_from_list op neutre (cst::le)


let neg_expr = function
  | Cst c -> Cst (-. c)
  | Unary (NEG, e) -> e
  | _ as e -> Unary (NEG, e)

let neg_list l = List.map neg_expr l

let rec get_mul_terms_expr = function
  | Binary(MUL, e1, e2) -> List.append (get_mul_terms_expr e1) (get_mul_terms_expr e2)
  | _ as e -> [e]

let rec get_add_terms_expr = function
  | Unary (NEG, e) -> neg_list (get_add_terms_expr e)
  | Binary (ADD, e1, e2) -> List.append (get_add_terms_expr e1) (get_add_terms_expr e2)
  | Binary (SUB, e1, e2) -> List.append (get_add_terms_expr e1) (neg_list (get_add_terms_expr e2))
  | Binary (MUL, e1, e2) as e -> let l = get_mul_terms_expr e in [get_lexpr l simplify_qexpr MUL one]
  | _ as e -> [e]

(* simplifies elementary function *)
let rec simplify_expr expr change =
  match expr with
  | Cst c -> (Cst c, change)
  | Var v -> (Var v, change)
  | Unary (NEG, e) ->
    (match e with
      | Cst 0. -> (Cst 0., true)
      | Cst a -> (Cst (-. a), true)
      | Unary (NEG, e') -> simplify_expr e' true
      | _ -> let (e', b) = simplify_expr e change in (Unary (NEG, e'), b)
    )
  | Unary (u, e) -> let (e, b) = simplify_expr e change in (Unary (u, e), b)
  | Binary (b, e1, e2) ->
    (match b with
     | MIN | MAX -> apply simplify_expr e1 e2 change b
     | ADD ->
       (match e1, e2 with
        | Cst a, Cst b -> (Cst (a +. b), true)
        | Cst 0., _ -> simplify_expr e2 change
        | _, Cst 0. -> simplify_expr e1 change
        | _, _ -> apply simplify_expr e1 e2 change ADD
       )
     | SUB ->
       (match e1, e2 with
        | Cst a, Cst b -> (Cst (a -. b), true)
        | Cst 0., _ -> let (e, _) = simplify_expr e2 change in (Unary (NEG, e), true)
        | _, Cst 0. -> simplify_expr e1 change
        | _, _ -> apply simplify_expr e1 e2 change SUB
       )
     | MUL ->
       (match e1, e2 with
        | Cst a, Cst b -> (Cst (a *. b), true)
        | Cst 0., _ | _, Cst 0. -> (Cst 0., true)
        | Cst 1., _ -> simplify_expr e2 change
        | _, Cst 1. -> simplify_expr e1 change
        | _, _ -> apply simplify_expr e1 e2 change MUL
       )
     | DIV ->
       (match e1, e2 with
        | _, Cst 0. -> (Cst 0., true) (* TODO treat NaN *)
        | Cst 0., _ -> (Cst 0., true)
        | Cst a, Cst b -> (Cst (a /. b), true)
        | _, Cst 1. -> simplify_expr e1 change
        | _, _ -> apply simplify_expr e1 e2 change DIV
       )
     | POW ->
       (match e1, e2 with
        | Cst a, Cst b -> (Cst (a ** b), true)
        | Cst 0., _ -> (Cst 0., true)
        | _, Cst 0. -> (Cst 1., true)
        | _, Cst 1. -> simplify_expr e1 change
        | _, _ -> apply simplify_expr e1 e2 change POW
       )
     | NROOT ->
       (match e1, e2 with
        | Cst 0., _ -> (Cst 0., true)
        | _, Cst 0. -> (Cst 1., true)
        | Cst a, Cst b -> (Cst (a ** (1. /.b)), true)
        | _, Cst 1. -> simplify_expr e1 change
        | _, _ -> apply simplify_expr e1 e2 change NROOT
       )
    )

let rec simplify_fp expr =
  let (e, b) = simplify_expr expr false in
  if b then
    simplify_fp e
  else
    (let lexpr = get_add_terms_expr e in
    get_lexpr lexpr simplify_lexpr ADD zero)


let rec simplify_bexpr = function
  | Cmp (op,e1,e2) -> Cmp (op, simplify_fp e1, simplify_fp e2)
  | And (b1,b2) -> And (simplify_bexpr b1, simplify_bexpr b2)
  | Or (b1,b2) -> Or (simplify_bexpr b1, simplify_bexpr b2)
  | Not b -> Not (simplify_bexpr b)

let left_hand_side (op, e1, e2) =
  match e1, e2 with
  | Cst 0., _ -> (inv op, e2)
  | _, Cst 0. -> (op, e1)
  | _, _ -> (op, simplify_fp (Binary (SUB, e1, e2)))

let rec left_hand = function
  | Cmp (op,e1,e2) -> left_hand_side (op, e1, e2)
  | And (b1,b2) | Or (b1,b2) -> left_hand b1
  | Not b -> left_hand b


(* derives a function regarding a variable *)
let rec derivate expr var =
  match expr with
  | Cst _ -> Cst 0.
  | Var v -> if String.equal var v then
               Cst 1.
             else
               Cst 0.
  | Unary (u, e) ->
    (match u with
     | NEG -> Unary (NEG, derivate e var)
     | SQRT -> Binary (DIV, derivate e var, Binary (MUL, Cst 2., Unary (SQRT, e)))
     | COS -> Unary (NEG, Binary (MUL, derivate e var, Unary (SIN, e)))
     | SIN -> Binary (MUL, derivate e var, Unary (COS, e))
     | TAN -> Binary (MUL, derivate e var, plus_one (sqr (Unary (TAN, e))))
     | COT -> Unary (NEG, Binary (MUL, derivate e var, plus_one (sqr (Unary (COT, e)))))
     | ASIN -> Binary (DIV, derivate e var, Unary (SQRT, Binary (SUB, one, (sqr e))))
     | ACOS -> Unary (NEG, Binary (DIV, derivate e var, Unary (SQRT, Binary (SUB, one, (sqr e)))))
     | ATAN -> Binary (DIV, derivate e var, plus_one (sqr e))
     | ACOT -> Unary (NEG, Binary (DIV, derivate e var, plus_one (sqr e)))
     | LN -> Binary (DIV, derivate e var, e)
     | LOG -> Binary (DIV, derivate e var, Binary (MUL, e, Unary (LN, Cst 10.)))
     | EXP -> Binary (MUL, derivate e var, Unary (EXP, e))
     | ABS -> (* TODO: depends if the variable is positive or negative *) derivate e var
    )
  | Binary (b, e1, e2) ->
    (match b with
     | ADD -> Binary (ADD, derivate e1 var, derivate e2 var)
     | SUB -> Binary (SUB, derivate e1 var, derivate e2 var)
     | MUL -> Binary (ADD, Binary (MUL, derivate e1 var, e2), Binary (MUL, e1, derivate e2 var))
     | DIV -> Binary (DIV, Binary (SUB, Binary (MUL, derivate e1 var, e2), Binary (MUL, e1, derivate e2 var)), sqr e2)
     | POW -> Binary (MUL, Binary (POW, e1, Binary (SUB, e2, one)), Binary (ADD, Binary (MUL, derivate e2 var, Binary (MUL, e1, Unary (LN, e1))), Binary (MUL, e2, derivate e1 var)))
     | NROOT -> Binary (DIV, derivate e1 var, Binary (MUL, e2, Binary (NROOT, Binary (POW, e1, Binary (SUB, e2, one)), e2)))
     | MIN | MAX -> Cst 0. (* TODO IMPLENTATION *)
    )

let rec derivative bexpr var =
  match bexpr with
  | Cmp (op,e1,e2) -> Cmp (op, derivate e1 var, derivate e2 var)
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
          let (op, e) = left_hand new_c in
          e
        else Cst 0.
      in
    (v, expr)::l
  ) [] vars

let compute_jacobian csp =
  List.fold_left (
    fun l c -> (c, ctr_jacobian c csp.init)::l
  ) [] csp.constraints


(*****************************************)
(*        PREPROCESSING FUNCTIONS        *)
(*****************************************)

let get_csts csp =
  let (csts, vars) = List.partition
    (fun (_, v, d) -> match d with
       | Finite (a, b) when a=b -> true
       | _ -> false
    ) csp.init in
  let cst = List.map
    (fun (_, v, d) -> match d with
       | Finite (a, b) when a=b -> (v, (a, b))
    ) csts in
  {csp with init = vars; constants = cst@csp.constants}

let rec replace_cst_expr (id, cst) expr =
  match expr with
  | Var v when v = id -> Cst cst
  | Unary (op, e) -> Unary (op, replace_cst_expr (id, cst) e)
  | Binary (op, e1, e2) -> Binary (op, replace_cst_expr (id, cst) e1, replace_cst_expr (id, cst) e2)
  | _ as e -> e

let rec replace_cst_bexpr cst = function
  | Cmp (op, e1, e2) -> Cmp (op, replace_cst_expr cst e1, replace_cst_expr cst e2)
  | And (b1, b2) -> And (replace_cst_bexpr cst b1, replace_cst_bexpr cst b2)
  | Or (b1, b2) -> Or (replace_cst_bexpr cst b1, replace_cst_bexpr cst b2)
  | Not b -> Not (replace_cst_bexpr cst b)


module Variables = Set.Make(struct type t=var let compare=compare end)

let rec get_vars_expr = function
  | Var v -> [v]
  | Cst c -> []
  | Unary (_, e) -> get_vars_expr e
  | Binary (_, e1, e2) -> List.append (get_vars_expr e1) (get_vars_expr e2)

let get_vars_set_expr expr = Variables.of_list (get_vars_expr expr)

let rec get_vars_bexpr = function
  | Cmp (_, e1, e2) -> List.append (get_vars_expr e1) (get_vars_expr e2)
  | And (b1, b2) -> List.append (get_vars_bexpr b1) (get_vars_bexpr b2)
  | Or (b1, b2) -> List.append (get_vars_bexpr b1) (get_vars_bexpr b2)
  | Not b -> get_vars_bexpr b

let get_vars_set_bexpr bexpr = Variables.of_list (get_vars_bexpr bexpr)

let get_vars_cstrs cstrs =
  List.map (fun c -> (c, get_vars_set_bexpr c)) cstrs

let replace_cst_cstrs (id, cst) cstrs =
  List.map (fun (c, vars) -> if Variables.mem id vars then
                               (replace_cst_bexpr (id, cst) c, Variables.remove id vars)
                             else (c, vars)
  ) cstrs

let replace_cst ctrs csts =
  List.fold_left
    (fun l (v, (a, b)) ->
      if a = b then replace_cst_cstrs (v, a) l
      else l
    ) ctrs csts


let get_c_expr l f op neutre =
  let (c, e) = List.partition is_cst l in
  let cst = to_fcst 0. c in
  let le = f [] e in
  (cst, expr_from_list op neutre le)

let rec simp_fp expr =
  let (e, b) = simplify_expr expr false in
  if b then
    simp_fp e
  else
    (let lexpr = get_add_terms_expr e in
    get_c_expr lexpr simplify_lexpr ADD zero)

let lh (op, e1, e2) =
  match e1, e2 with
  | Cst 0., _ -> let (c, e) = simp_fp e2 in (inv op, c, e)
  | _, Cst 0. -> let (c, e) = simp_fp e1 in (op, c, e)
  | _, _ -> let (c, e) = simp_fp (Binary (SUB, e1, e2)) in (op, c, e)


                                                         

let filter_cstrs ctr_vars consts =
  List.fold_left (fun (cstr, csts, b) (c, v) ->
  if Variables.cardinal v = 1 then
    match c with
    | Cmp (EQ, e1, e2) ->
       (let (op', cst, e) = lh (EQ, e1, e2) in
        let negc = if cst = 0. then 0. else -. cst in
        match e with
        | Var var -> (cstr, (var, (negc, negc))::csts, true)
        | Unary(NEG, Var var) -> (cstr, (var, (cst, cst))::csts,  true)
        | Binary(MUL, Var var, Cst a) | Binary(MUL, Cst a, Var var) -> (cstr, (var, (negc/.a, negc/.a))::csts, true)
        | Unary(NEG, (Binary(MUL, Var var, Cst a))) |  Unary(NEG, (Binary(MUL, Cst a, Var var))) -> (cstr, (var, (cst/.a, cst/.a))::csts, true)
        | _ -> ((c, v)::cstr, csts, b))
    | _ -> ((c, v)::cstr, csts, b)
  else ((c, v)::cstr, csts, b)
  ) ([], consts, false) ctr_vars

let rec repeat ctr_vars csts =
  let ctrs = replace_cst ctr_vars csts in
  let (ctrs', csts', b) = filter_cstrs ctrs csts in
  if b then
    repeat ctrs' csts'
  else
    (ctrs', csts')


  
let rec view e1 e2 =
  match e1, e2 with
  | Var(v), _ ->(v, e2)
  | _, Var(v) -> (v, e1)
  | Unary(NEG, n), e -> view n (simplify_fp (Unary(NEG, e)))
  | Binary(ADD, a1, a2), e ->
     if has_variable a1 then view a1 (simplify_fp (Binary(SUB, e, a2)))
     else view a2 (simplify_fp (Binary(SUB, e, a1)))
  | Binary(SUB, s1, s2), e -> 
     if has_variable s1 then view s1 (simplify_fp (Binary(ADD, e, s2)))
     else view s2 (simplify_fp (Binary(SUB, s1, e)))
  | Binary(MUL, m1, m2), e -> 
     if has_variable m1 then view m1 (simplify_fp (Binary(DIV, e, m2)))
     else view m2 (simplify_fp (Binary(DIV, e, m1)))
  | Binary(DIV, d1, d2), e -> 
     if has_variable d1 then view d1 (simplify_fp (Binary(MUL, e, d2)))
     else view d2 (simplify_fp (Binary(MUL, e, d1)))
  | _, _ -> Format.printf "NOOOOOOOOOOOOO\n"; ("NOPE", Binary(SUB, e1, e2))

let rec replace_view_expr ((id, e) as view) expr =
  match expr with
  | Var v when v = id -> e
  | Unary (op, u) -> Unary (op, replace_view_expr view u)
  | Binary (op, b1, b2) -> Binary (op, replace_view_expr view b1, replace_view_expr view b2)
  | _ as expr -> expr

let rec replace_view_bexpr view = function
  | Cmp (op, e1, e2) -> Cmp(op, replace_view_expr view e1, replace_view_expr view e2)
  | And (b1, b2) -> And (replace_view_bexpr view b1, replace_view_bexpr view b2)
  | Or (b1, b2) -> Or (replace_view_bexpr view b1, replace_view_bexpr view b2)
  | Not b -> Not (replace_view_bexpr view b)

let replace_view_ctr ((id, e) as view) ctrs =
  List.map (fun (c, vars) -> if Variables.mem id vars then
                               (replace_view_bexpr view c, Variables.remove id vars)
                             else (c, vars)
    ) ctrs

let replace_view ctrs views =
  List.fold_left (fun l v -> replace_view_ctr v l) ctrs views

let rec rep_view view views =
  List.map (fun (id, e) -> (id, replace_view_expr view e)) views

let rep_in_view (id, e) views =
  List.fold_left (fun (id, e) v -> (id, replace_view_expr v e)) (id, e) views


  
let get_vars_jacob jacob =
  List.map (fun (c, j) -> (c, get_vars_set_bexpr c, j)) jacob

let replace_cst_jacob (id, cst) jacob =
  List.map (fun (c, vars, j) ->
    if Variables.mem id vars then
      (replace_cst_bexpr (id, cst) c, Variables.remove id vars, j)
    else (c, vars, j)
  ) jacob



(*****************************************)
(*        USEFUL FUNCTION ON AST         *)
(*****************************************)

let empty = {init = []; constraints= []; constants=[]; objective =Cst(0.); to_draw=[]; jacobian = []; view = []}

let get_vars p =
  List.map (fun (_,v,_) -> v) p.init

let add_real_var csp name inf sup =
  let assign = (REAL, name, (Finite(inf,sup))) in
  {csp with init = assign::csp.init}

let add_constr csp c =
  let jac = List.map (
    fun (_, v, _) ->
    let new_c = simplify_bexpr (derivative c v) in
    let (op, expr) = left_hand new_c in
    (v, expr)
  ) csp.init in
  {csp with constraints = c::csp.constraints; jacobian = (c, jac)::csp.jacobian}

let domain_to_constraints (_,v,d)  =
  let c1, c2 = match d with
  | Finite (l,h) ->
     (Var v, GEQ, Cst l), (Var v, LEQ, Cst h)
  | Minf (i) ->
     (Var v, GEQ, Cst neg_infinity), (Var v, LEQ, Cst i)
  | Inf (i) ->
     (Var v, GEQ, Cst i), (Var v, LEQ, Cst infinity)
  | _ ->
     (Var v, GEQ, Cst neg_infinity), (Var v, LEQ, Cst infinity)
  in c1,c2

(* iter on expr*)
let rec iter_expr f = function
  | Binary (op,e1,e2) as b -> f b; iter_expr f e1; iter_expr f e2
  | Unary (uop,e) as u -> f u; iter_expr f e
  | x -> f x

(* iter on constraints *)
let rec iter_constr f_expr f_constr = function
  | Cmp (c,e1,e2) as constr -> f_constr constr; iter_expr f_expr e1; iter_expr f_expr e2
  | And (b1,b2) as constr -> f_constr constr; iter_constr f_expr f_constr b1; iter_constr f_expr f_constr b2
  | Or  (b1,b2) as constr -> f_constr constr; iter_constr f_expr f_constr b1; iter_constr f_expr f_constr b2
  | Not b as constr -> f_constr constr; iter_constr f_expr f_constr b

(* power unrolling on exprs *)
let rec power_unrolling expr : expr =
  let rec doit res e1 i =
    match i with
    | 0 -> Cst 1.
    | 1 -> res
    | _ -> doit (Binary(MUL,res,res)) e1 (i-1)
  in
  match expr with
  | Binary (POW,e1,Cst x) when ceil x = x && x >= 0. -> doit e1 e1 (int_of_float x)
  | Unary (u, e) -> Unary (u,(power_unrolling e))
  | Binary (b,e1,e2) -> Binary(b,(power_unrolling e1), (power_unrolling e2))
  | x -> x

(* power unrolling on bexprs *)
let rec power_unrolling_bexpr bexpr : bexpr =
  match bexpr with
  | Cmp (c,e1,e2) -> Cmp(c, (power_unrolling e1), (power_unrolling e2))
  | And (b1,b2) -> And (power_unrolling_bexpr b1, power_unrolling_bexpr b2)
  | Or  (b1,b2) -> Or (power_unrolling_bexpr b1, power_unrolling_bexpr b2)
  | Not b -> Not (power_unrolling_bexpr b)

(* cmp operator negation *)
let neg = function
  | EQ -> NEQ
  | LEQ ->GT
  | GEQ ->LT
  | NEQ ->EQ
  | GT -> LEQ
  | LT -> GEQ

(* constraint negation *)
let rec neg_bexpr = function
  | Cmp (op,e1,e2) -> Cmp(neg op,e1,e2)
  | And (b1,b2) -> Or (neg_bexpr b1, neg_bexpr b2)
  | Or (b1,b2) -> And (neg_bexpr b1, neg_bexpr b2)
  | Not b -> b


(*************************************************************)
(*                    PRINTING UTILITIES                     *)
(*************************************************************)

let print_unop fmt = function
  | NEG -> Format.fprintf fmt "-"
  | SQRT -> Format.fprintf fmt "sqrt"
  | COS -> Format.fprintf fmt "cos"
  | SIN -> Format.fprintf fmt "sin"
  | TAN -> Format.fprintf fmt "tan"
  | COT -> Format.fprintf fmt "cot"
  | ASIN -> Format.fprintf fmt "asin"
  | ACOS -> Format.fprintf fmt "acos"
  | ATAN -> Format.fprintf fmt "atan"
  | ACOT -> Format.fprintf fmt "acot"
  | LN -> Format.fprintf fmt "ln"
  | LOG -> Format.fprintf fmt "log"
  | EXP -> Format.fprintf fmt "exp"
  | ABS -> Format.fprintf fmt "abs"

let print_binop fmt = function
  | ADD -> Format.fprintf fmt "+"
  | SUB -> Format.fprintf fmt "-"
  | MUL -> Format.fprintf fmt "*"
  | DIV -> Format.fprintf fmt "/"
  | POW -> Format.fprintf fmt "^"
  | MIN -> Format.fprintf fmt "min"
  | MAX -> Format.fprintf fmt "max"
  | NROOT -> Format.fprintf fmt "nroot"

let print_cmpop fmt = function
  | EQ -> Format.fprintf fmt "="
  | LEQ -> Format.fprintf fmt "<="
  | GEQ -> Format.fprintf fmt ">="
  | NEQ -> Format.fprintf fmt "<>"
  | GT ->  Format.fprintf fmt ">"
  | LT -> Format.fprintf fmt "<"

let print_typ fmt = function
  | INT ->  Format.fprintf fmt "int"
  | REAL ->  Format.fprintf fmt "real"

let print_var fmt s = Format.fprintf fmt "%s" s

let print_dom fmt = function
  | Finite (a,b) ->  Format.fprintf fmt "[%.2f; %.2f]" a b
  | Minf i -> Format.fprintf fmt "[-oo; %.2f]" i
  | Inf i -> Format.fprintf fmt "[%.2f; 00]" i
  | Top -> Format.fprintf fmt "[-oo; 00]"

let print_assign fmt (a,b,c) =
  Format.fprintf fmt "%a %a=%a" print_typ a print_var b print_dom c

let print_cst fmt (a, b) =
  match (a, b) with
  | (a, b)  when a = b ->  Format.fprintf fmt "%f" a
  | (a, b) -> Format.fprintf fmt "[%f; %f]" a b

let print_csts fmt (a, b) =
  Format.fprintf fmt "%a = %a" print_var a print_cst b




let print_aux_csts fmt (a, (b, c)) =
  Format.fprintf fmt "%a:[%f;%f]" print_var a b c

let rec print_all_csts fmt = function
  | [] -> ()
  | a::[] -> Format.fprintf fmt "%a" print_aux_csts a
  | a::tl -> Format.fprintf fmt "%a " print_aux_csts a; print_all_csts fmt tl


let rec print_expr fmt = function
  | Unary (NEG, e) ->
    Format.fprintf fmt "(- %a)" print_expr e
  | Unary (u, e) ->
    Format.fprintf fmt "%a (%a)" print_unop u print_expr e
  | Binary (b, e1 , e2) ->
    Format.fprintf fmt "(%a %a %a)" print_expr e1 print_binop b print_expr e2
  | Var v -> Format.fprintf fmt "%s" v
  | Cst c -> Format.fprintf fmt "%.2f" c

let rec print_bexpr fmt = function
  | Cmp (c,e1,e2) ->
    Format.fprintf fmt "%a %a %a" print_expr e1 print_cmpop c print_expr e2
  | And (b1,b2) ->
    Format.fprintf fmt "%a && %a" print_bexpr b1 print_bexpr b2
  | Or  (b1,b2) ->
    Format.fprintf fmt "%a || %a" print_bexpr b1 print_bexpr b2
  | Not b -> Format.fprintf fmt "not %a" print_bexpr b

let print_jacob fmt (v, e) =
  Format.fprintf fmt "\t(%a, %a)" print_var v print_expr e

let rec print_jacobian fmt = function
  | [] -> ()
  | (c, j)::tl -> Format.fprintf fmt "%a;\n" print_bexpr c; (* List.iter (print_jacob fmt) j; Format.fprintf fmt "\n";*) print_jacobian fmt tl


let print_view fmt (v, e) =
  Format.fprintf fmt "%a = %a" print_var v print_expr e

let print fmt prog =
  let rec aux f = function
  | [] -> ()
  | a::tl -> Format.fprintf fmt "%a;\n" f a; aux f tl
  in
  aux print_assign prog.init;
  Format.fprintf fmt "\n";
  aux print_csts prog.constants;
  Format.fprintf fmt "\n";
  aux print_view prog.view;
  Format.fprintf fmt "\n";
  print_jacobian fmt prog.jacobian;
  Format.fprintf fmt "\n"

let get_views ctr_vars =
  List.fold_left (fun (ctrs, views) (c, v) ->
  if ((is_cons_linear c) && (Variables.cardinal v = 2)) then
    match c with
    | Cmp (EQ, e1, e2) ->
       let (_, cst, e) = lh (EQ, e1, e2) in
       let value = if cst = 0. then 0. else -. cst in
       let new_view = rep_in_view (view e (Cst value)) views in
       let views' = rep_view new_view views in
      (ctrs, new_view::views')
    | _ -> ((c, v)::ctrs, views)
  else ((c, v)::ctrs, views)
    ) ([], []) ctr_vars
  
let rec find_all_views ctrs =
  let (ctrs, views) = get_views ctrs in
  if List.length views > 0 then
    let ctrs' = replace_view ctrs views in
    let (v, c) = find_all_views ctrs' in
    (views@v, c)
  else
    ([], ctrs)
  
                  
  

let simplify csp =
  let p = get_csts csp in
  let ctr_vars = get_vars_cstrs p.constraints in
  let (ctrs, csts) = repeat ctr_vars p.constants in

  let (views, ctrs') = find_all_views ctrs in
  
  
  let (ctrs, _) = List.split ctrs' in
  let (cons, _) = List.split csts in
  let (view, _) = List.split views in
  let not_vars = cons@view in
  let vars = List.filter (fun (t, v, d) -> not(List.mem v not_vars)) p.init in
  {p with init = vars; constants = csts; constraints = ctrs; view = views}

