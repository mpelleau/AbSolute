open Apron
open Csp
open Apron_utils

module type ADomain = sig
  type t
  val get_manager: t Manager.t
end

(* Translation functor for syntax.prog to apron values*)
module SyntaxTranslator (D:ADomain) = struct
  let man = D.get_manager

  let rec expr_to_apron a (e:expr) : Texpr1.expr =
    let env = Abstract1.env a in
    match e with
    | Var v ->
      let var = Var.of_string v in
      if not (Environment.mem_var env var)
      then failwith ("variable not found: "^v);
      Texpr1.Var var
    | Cst c -> Texpr1.Cst (Coeff.s_of_float c)
    | Unary (o,e1) ->
      let r = match o with
	      | NEG  -> Texpr1.Neg
	      | SQRT -> Texpr1.Sqrt
	      | COS  -> failwith "COS unsupported with apron"
        | SIN  -> failwith "SIN unsupported with apron"
        | TAN  -> failwith "TAN unsupported with apron"
	      | COT  -> failwith "COT unsupported with apron"
	      | ACOS -> failwith "ACOS unsupported with apron"
        | ASIN -> failwith "ASIN unsupported with apron"
        | ATAN -> failwith "ATAN unsupported with apron"
	      | ACOT -> failwith "ACOT unsupported with apron"
	      | ABS  -> failwith "ABS unsupported with apron"
        | LN   -> failwith "LN unsupported with apron"
        | LOG  -> failwith "LOG unsupported with apron"
        | EXP  -> failwith "EXP unsupported with apron"
      in
      let e1 = expr_to_apron a e1 in
      Texpr1.Unop (r, e1, Texpr1.Real, Texpr1.Near)
    | Binary (o,e1,e2) ->
       let r = match o with
	       | ADD -> Texpr1.Add
	       | SUB -> Texpr1.Sub
	       | DIV -> Texpr1.Div
	       | MUL -> Texpr1.Mul
	       | POW -> Texpr1.Pow
               | MIN -> failwith "MIN unsupported with apron"
               | MAX -> failwith "MAX unsupported with apron"
               | NROOT -> failwith "NROOT unsupported with apron"
       in
       let e1 = expr_to_apron a e1
       and e2 = expr_to_apron a e2 in
       Texpr1.Binop (r, e1, e2, Texpr1.Real, Texpr1.Near)

  let cmp_expr_to_tcons b env =
    let cmp_to_apron (e1,op,e2) =
      match op with
      | EQ  -> e1, e2, Tcons1.EQ
      | NEQ -> e1, e2, Tcons1.DISEQ
      | GEQ -> e1, e2, Tcons1.SUPEQ
      | GT  -> e1, e2, Tcons1.SUP
      | LEQ -> e2, e1, Tcons1.SUPEQ
      | LT  -> e2, e1, Tcons1.SUP
    in
    let e1,e2,op = cmp_to_apron b in
    let e = Binary (SUB, e1, e2) in
    let a = Abstract1.top man env in
    let e = Texpr1.of_expr env (expr_to_apron a e) in
    let res = Tcons1.make e op in
    res

  let apron_to_var abs =
    let env = Abstract1.env abs in
    let (iv, rv) = Environment.vars env in
    let ivars = Array.map (fun v -> Var.to_string v) iv in
    let rvars = Array.map (fun v -> Var.to_string v) rv in
    (Array.to_list ivars, Array.to_list rvars)

  let rec apron_to_expr texpr env =
    match texpr with
    | Texpr1.Cst c -> Cst (coeff_to_float c)
    | Texpr1.Var v ->
      let e = match (Environment.typ_of_var env v) with
              | Environment.INT -> Var ((Var.to_string v)^"%")
              | Environment.REAL -> Var (Var.to_string v)
      in e
    | Texpr1.Unop (op, e, _, _) ->
      let o = match op with
              | Texpr1.Neg -> NEG
              | Texpr1.Sqrt -> SQRT
              | Texpr1.Cast -> failwith "Cast unsupported with AbSolute"
      in
      let e = apron_to_expr e env in
      Unary (o, e)
    | Texpr1.Binop (op, e1, e2, _, _) ->
      let o = match op with
        | Texpr1.Add -> ADD
        | Texpr1.Sub -> SUB
        | Texpr1.Mul -> MUL
        | Texpr1.Div -> DIV
        | Texpr1.Mod -> failwith "Mod not yet supported with AbSolute"
        | _ -> failwith "operation not yet supported with AbSolute"
      in
      let e1 = apron_to_expr e1 env
      and e2 = apron_to_expr e2 env in
      Binary (o, e1, e2)

  let apron_to_bexpr tcons env =
    let apron_to_cmp op =
      match op with
      | Tcons1.EQ  -> EQ
      | Tcons1.DISEQ -> NEQ
      | Tcons1.SUPEQ -> GEQ
      | Tcons1.SUP -> GT
      | _ -> failwith "operation not yet supported with AbSolute"
    in
    let typ = apron_to_cmp (Tcons1.get_typ tcons) in
    let exp = apron_to_expr (Texpr1.to_expr (Tcons1.get_texpr1 tcons)) env in
    (exp, typ, Cst (0.))

  let apron_to_bexpr abs =
    let abscons = Abstract1.to_tcons_array man abs in
    let earray = abscons.Tcons1.tcons0_array in
    let tenv = abscons.Tcons1.array_env in
    Array.map (fun t ->
                apron_to_bexpr (Tcons1.{tcons0 = t; env = tenv}) tenv
              ) earray
    |> Array.to_list




end


(*****************************************************************)
(* Some types and values that all the domains of apron can share *)
(* These are generic and can be redefined in the actuals domains *)
(*****************************************************************)
module MAKE(AP:ADomain) = struct

  module A = Abstractext

  type t = AP.t A.t

  let man = AP.get_manager

  module T = SyntaxTranslator(AP)

  let empty = A.top man (Environment.make [||] [||])

  let vars abs =
    let (ivars, rvars) = Environment.vars (A.env abs) in
    let tmp = (Array.to_list ivars)@(Array.to_list rvars) in
    let tmp' = List.map (Var.to_string) tmp in
    List.sort_uniq (compare) tmp'

  let add_var abs (typ,v) =
    let e = A.env abs in
    let ints,reals = if typ = INT then [|Var.of_string v|],[||] else [||],[|Var.of_string v|] in
    let env = Environment.add e ints reals in
    A.change_environment man abs env false

  let var_bounds abs v =
    let var = Var.of_string v in
    let i = A.bound_variable man abs var in
    itv_to_float i

  let bounded_vars abs =
    let (ivars, rvars) = Environment.vars (A.env abs) in
    let vars = (Array.to_list ivars)@(Array.to_list rvars) in
    let itvs = List.fold_left (fun l v ->
      (Var.to_string v, itv_to_float (A.bound_variable man abs v))::l
      ) [] vars in
    List.filter (fun (v, (l, u)) -> l = u) itvs

  let rem_var abs v =
    let var = Var.of_string v in
    let e = Environment.remove (A.env abs) (Array.of_list [var]) in
    A.change_environment man abs e false

  let is_bottom abs =
    A.is_bottom man abs

  let is_singleton b v =
    let man = A.manager b in
    if A.is_bottom man b then true
    else
      let itv = A.bound_variable man b v  in
      diam_interval itv |> Mpqf.to_float = 0.

  let is_enumerated abs =
    let int_vars = Environment.vars (A.env abs) |> fst in
    try
      Array.iter (fun v -> if (is_singleton abs v) |> not then raise Exit) int_vars;
      true
    with Exit -> false

  let join a b = A.join man a b

  let prune a b =
    let work acc a c =
      let neg_c = Linconsext.neg c in
      let a' = A.filter_lincons man a c
	    and s = A.filter_lincons man a neg_c in
	    if is_bottom s then a,acc
	    else a',(s::acc)
    in
    let a,pruned = Linconsext.array_fold (fun (abs,acc) c ->
      if Linconsext.get_typ c = Linconsext.EQ then
        let c1,c2 = Linconsext.spliteq c in
        let a',acc' = work acc a c1 in
        work acc' a' c2
      else work acc a c
    ) (a,[]) (A.to_lincons_array man b) in pruned,b

  let filter b (e1,c,e2) =
    (*Format.printf "%a\n" A.print b;*)
    let env = A.env b in
    let c = T.cmp_expr_to_tcons (e1,c,e2) env in
    if Tconsext.get_typ c = Tconsext.DISEQ then
      let lin = Tconsext.to_lincons b.A.env c in
      let l1,l2 = Linconsext.splitdiseq lin in
      join (A.filter_lincons man b l1) (A.filter_lincons man b l2)
    else A.filter_tcons man b c

  let filterl b (e1,c,e2) =
    filter b (e1,c,e2)

  let print = A.print

  let to_box abs env =
    let abs' = A.change_environment man abs env false in
    A.to_lincons_array man abs' |>
    A.of_lincons_array (Box.manager_alloc ()) env

  let to_oct abs env =
    let abs' = A.change_environment man abs env false in
    A.to_lincons_array man abs' |>
    A.of_lincons_array (Oct.manager_alloc ()) env

  let to_poly abs env =
    let abs' = A.change_environment man abs env false in
    A.to_lincons_array man abs' |>
    A.of_lincons_array (Polka.manager_alloc_strict ()) env

  let forward_eval abs cons =
    let ap_expr = T.expr_to_apron abs cons |> Texpr1.of_expr (A.env abs) in
    let obj_itv = A.bound_texpr man abs ap_expr in
    let obj_inf = obj_itv.Interval.inf
    and obj_sup = obj_itv.Interval.sup in
    (scalar_to_float obj_inf, scalar_to_float obj_sup)

    (* utilties for splitting *)

  let rec largest tab i max i_max =
    if i>=Array.length tab then (max, i_max)
    else
	    let dim = diam_interval (tab.(i)) in
	    if Mpqf.cmp dim max > 0 then largest tab (i+1) dim i
	    else largest tab (i+1) max i_max

  let largest abs : (Var.t * Interval.t * Mpqf.t) =
    let env = A.env abs in
    let box = A.to_box man abs in
    let tab = box.A.interval_array in
    let rec aux cur i_max diam_max itv_max =
      if cur>=Array.length tab then (i_max, diam_max, itv_max)
      else
        let e = tab.(cur) in
        let diam = diam_interval e in
        if Mpqf.cmp diam diam_max > 0 then aux (cur+1) cur diam e
        else aux (cur+1) i_max diam_max itv_max
    in
    let (a,b,c) = aux 0 0 (Mpqf.of_int 0) tab.(0) in
    ((Environment.var_of_dim env a),c,b)

    (* Compute the minimal and the maximal diameter of an array on intervals *)
    let rec minmax tab i max i_max min i_min =
      if i>=Array.length tab then  (max, i_max, min, i_min)
      else
	let dim = diam_interval (tab.(i)) in
	if Mpqf.cmp dim max > 0 then minmax tab (i+1) dim i min i_min
	else if Mpqf.cmp min dim > 0 then minmax tab (i+1) max i_max dim i
	else minmax tab (i+1) max i_max min i_min

    (* let p1 = (p11, p12, ..., p1n) and p2 = (p21, p22, ..., p2n) two points
     * The vector p1p2 is (p21-p11, p22-p12, ..., p2n-p1n) and the orthogonal line
     * to the vector p1p2 passing by the center of the vector has for equation:
     * (p21-p11)(x1-b1) + (p22-p12)(x2-b2) + ... + (p2n-p1n)(xn-bn) = 0
     * with b = ((p11+p21)/2, (p12+p22)/2, ..., (p1n+p2n)/2) *)
    let rec genere_linexpr gen_env size p1 p2 i list1 list2 cst =
      if i >= size then (list1, list2, cst) else
	      let ci = p2.(i) -. p1.(i) in
	      let cst' = cst +. ((p1.(i) +. p2.(i)) *. ci) in
	      let ci' = 2. *. ci in
	      let coeffi = Coeff.Scalar (Scalar.of_float ci') in
	      let list1' = List.append list1 [(coeffi, Environment.var_of_dim gen_env i)] in
	      let list2' = List.append list2 [(Coeff.neg coeffi, Environment.var_of_dim gen_env i)] in
	      genere_linexpr gen_env size p1 p2 (i+1) list1' list2' cst'

 let split abs (e1,e2) =
   let meet_linexpr abs man env expr =
     let cons = Linconsext.make expr Linconsext.SUPEQ in
     A.filter_lincons man abs cons
   in
   let env = A.env abs in
   let abs1 = meet_linexpr abs man env e1 in
   let abs2 = meet_linexpr abs man env e2 in
   [abs1; abs2]

  (************************************************)
  (* POLYHEDRIC VERSION OF SOME USEFUL OPERATIONS *)
  (************************************************)

  let get_expr man (polyad:Polka.strict Polka.t A.t) =
    let poly = A.to_generator_array man polyad in
    let gen_env = poly.Generator1.array_env in
    (*print_gen gens gen_env;*)
    let size = Environment.size gen_env in
    let gen_float_array = gen_to_array poly size in
    let (p1, i1, p2, i2, dist_max) = maxdisttab gen_float_array in
    let (list1, list2, cst) = genere_linexpr gen_env size p1 p2 0 [] [] 0. in
    let cst_sca1 = Scalar.of_float (-1. *.(cst +. split_prec)) in
    let cst_sca2 = Scalar.of_float (cst +. split_prec) in
    let linexp = Linexpr1.make gen_env in
    Linexpr1.set_list linexp list1 (Some (Coeff.Scalar cst_sca1));
    let linexp' = Linexpr1.make gen_env in
    Linexpr1.set_list linexp' list2 (Some (Coeff.Scalar cst_sca2));
    (linexp, linexp')

  let is_small man polyad =
    let poly = A.to_generator_array man polyad in
    let gen_env = poly.Generator1.array_env in
    (*print_gen gens gen_env;*)
    let size = Environment.size gen_env in
    let gen_float_array = gen_to_array poly size in
    let (p1, i1, p2, i2, dist_max) = maxdisttab gen_float_array in
    (dist_max <= !Constant.precision)
end
