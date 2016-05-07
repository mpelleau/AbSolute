open Apron
open Syntax
open Utils

module type ADomain = sig 
  type t
  val get_manager: t Manager.t 
end


(* Translation functor for syntax.prog to apron values*)
module SyntaxTranslator (D:ADomain) = struct
  let man = D.get_manager

  let type_to_apron = function
    | REAL -> Texpr1.Real
    | INT -> Texpr1.Int

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
	| NEG -> Texpr1.Neg
	| SQRT -> Texpr1.Sqrt
	| COS | SIN | ABS -> failwith "COS and SIN unsupported with apron"
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
      in
      let e1 = expr_to_apron a e1 
      and e2 = expr_to_apron a e2 in
      Texpr1.Binop (r, e1, e2, Texpr1.Real, Texpr1.Near)
	
  let cmp_expr_to_apron b env =
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
    
   let interval_of_dom env t v = function
    | Finite (l,h) -> Interval.of_float l h
    | Top -> Interval.top
    | Minf i -> Interval.of_scalar (Scalar.of_infty (-1)) (Scalar.of_float i)
    | Inf i -> Interval.of_scalar (Scalar.of_float i) (Scalar.of_infty 1)

  let domain_to_apron p =
    let integers,reals = List.partition (fun (a,_,_) -> a = INT) p.init in
    let integers = List.map (fun (_,v,_) -> Var.of_string v) integers |> Array.of_list
    and reals = List.map (fun (_,v,_) -> Var.of_string v) reals |> Array.of_list in
    let env = Environment.make integers reals in
    let var_array = List.map (fun (_,v,_) -> Var.of_string v) p.init |> Array.of_list in
    let i_array = Array.make (List.length p.init) (Interval.of_int 0 0) in
    List.iteri (fun i (t,v,dom) ->
      let itv = interval_of_dom env t v dom in
      i_array.(i) <- itv
    ) p.init;
    Abstract1.of_box man env var_array i_array
end


(*****************************************************************)
(* Some types and values that all the domains of apron can share *)
(* These are generic and can be redefined in the actuals domains *)
(*****************************************************************)
module MAKE(AP:ADomain) = struct
  type t = AP.t Abstract1.t

  let man = AP.get_manager

  type split = Linexpr1.t

  module Translate = SyntaxTranslator(AP)

  let of_problem p = Translate.domain_to_apron p
      
  let is_bottom b = Abstract1.is_bottom man b

  let is_singleton b v =
    let man = Abstract1.manager b in
    if Abstract1.is_bottom man b then true
    else 
      let itv = Abstract1.bound_variable man b v  in
      Utils.diam_interval itv |> Mpqf.to_float = 0.

  let is_enumerated abs =
    let int_vars = Environment.vars (Abstract1.env abs) |> fst in
    try 
      Array.iter (fun v -> if (is_singleton abs v) |> not then raise Exit) int_vars;
      true
    with Exit -> false

  let join a b = Abstract1.join man a b
    
  let filter b (e1,c,e2) = 
    let env = Abstract1.env b in
    let c = Translate.cmp_expr_to_apron (e1,c,e2) env in
    Abstract1.meet_tcons_array man b (Utils.tcons_list_to_earray [c])
      
  let print = Abstract1.print

  let split abs list =
    let meet_linexpr abs man env expr =
      let cons = Lincons1.make expr Lincons1.SUPEQ in
      let tab = Lincons1.array_make env 1 in
      Lincons1.array_set tab 0 cons;
      let abs' = Abstract1.meet_lincons_array man abs tab in
      abs'
    in
    let env = Abstract1.env abs in
    let abs1 = meet_linexpr abs man env (List.nth list 0) in
    let abs2 = meet_linexpr abs man env (List.nth list 1) in
    [abs1; abs2]    

  let to_box abs env = 
    let abs' = Abstract1.change_environment man abs env false in
    Abstract1.to_lincons_array man abs' |>
    Abstract1.of_lincons_array (Box.manager_alloc ()) env

  let to_oct abs env =
    let abs' = Abstract1.change_environment man abs env false in
    Abstract1.to_lincons_array man abs' |>
    Abstract1.of_lincons_array (Oct.manager_alloc ()) env

  let to_poly abs env =
    let abs' = Abstract1.change_environment man abs env false in
    Abstract1.to_lincons_array man abs' |>
    Abstract1.of_lincons_array (Polka.manager_alloc_strict ()) env

  (* given two variables to draw, and an environnement, 
     returns the two variables value in the environment.  
  *)
  let get_indexes env vars = 
    let i1,i2 = match vars with
      | None -> (0,1)
      | Some (x,y) ->
	(Environment.dim_of_var env (Var.of_string x)),
	(Environment.dim_of_var env (Var.of_string y))
    in i1,i2
    
  let points_to_draw abs vars =
    let env = Abstract1.env abs in
    let draw_pol pol =
      let i1,i2 = get_indexes env vars in
      let x = Environment.var_of_dim env i1 
      and y = Environment.var_of_dim env i2 in
      let get_coord l = (Linexpr1.get_coeff l x),(Linexpr1.get_coeff l y) in      
      let gen' = Abstract1.to_generator_array (Polka.manager_alloc_strict ()) pol in
      let v = Array.init (Generator1.array_length gen')
	(fun i -> get_coord 
	  (Generator1.get_linexpr1 (Generator1.array_get gen' i)))
	       |> Array.to_list
      in 
      List.map (fun(a,b)-> (Utils.coeff_to_float a, Utils.coeff_to_float b)) v
    in 
    draw_pol (to_poly abs env) 


    let forward_eval abs cons = 
      let obj_itv = Abstract1.bound_texpr man abs (Texpr1.of_expr (Abstract1.env abs) (Translate.expr_to_apron abs cons)) in
      let obj_inf = obj_itv.Interval.inf
      and obj_sup = obj_itv.Interval.sup in
      let open Utils in
      (scalar_to_float obj_inf, scalar_to_float obj_sup)

     (* utilties for splitting *)

    let rec largest tab i max i_max =
      if i>=Array.length tab then (max, i_max) 
      else
	let dim = diam_interval (tab.(i)) in
	if Mpqf.cmp dim max > 0 then largest tab (i+1) dim i
	else largest tab (i+1) max i_max
	  
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
end
