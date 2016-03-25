open Apron
open Syntax

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
	| COS | SIN -> failwith "COS and SIN unsupported with apron"
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
         
  let rec bexpr_to_apron b env =
    match b with
    | Cmp (cmp, e1, e2) -> cmp_expr_to_apron (e1,cmp,e2) env
    | _ -> failwith "NOT, OR and AND are not implemented yet"
    
  let abstract_of_finite a v (l,h) =
    let env = Abstract1.env a in
    let v = Var.of_string v in
    let ar = Lincons1.array_make env 2 in
    if l <> neg_infinity then (
      let c1 = Lincons1.make (Linexpr1.make env) Lincons1.SUPEQ in
      Lincons1.set_list c1 [Coeff.s_of_float 1., v] (Some (Coeff.s_of_float (-. l)));
      Lincons1.array_set ar 0 c1
    );
    if h <> infinity then (
      let c2 = Lincons1.make( Linexpr1.make env) Lincons1.SUPEQ in
      Lincons1.set_list c2 [Coeff.s_of_float (-1.), v] (Some (Coeff.s_of_float h));
      Lincons1.array_set ar 1 c2
    );
    Abstract1.meet_lincons_array man a ar

  let abstract_of_dom a v = function
    | Finite (l,h) -> abstract_of_finite a v (l,h)
    | Top -> Abstract1.(top man a.env)
    | Minf i ->
      let v = Var.of_string v in
      let ar = Lincons1.array_make a.Abstract1.env 1 in
      let c2 = Lincons1.make( Linexpr1.make a.Abstract1.env) Lincons1.SUPEQ in
      Lincons1.set_list c2 [Coeff.s_of_float (-1.), v] (Some (Coeff.s_of_float i));
      Lincons1.array_set ar 0 c2;
      Abstract1.meet_lincons_array man a ar
    | Inf i ->      
      let v = Var.of_string v in
      let ar = Lincons1.array_make a.Abstract1.env 1 in
      let c1 = Lincons1.make( Linexpr1.make a.Abstract1.env) Lincons1.SUPEQ in
      Lincons1.set_list c1 [Coeff.s_of_float 1., v] (Some (Coeff.s_of_float (-. i)));
      Lincons1.array_set ar 0 c1;
      Abstract1.meet_lincons_array man a ar

  let to_apron p =
    let integers,reals = List.partition (fun (a,_,_) -> a = INT) p.init in
    let integers = List.map (fun (_,v,_) -> Var.of_string v) integers |> Array.of_list
    and reals = List.map (fun (_,v,_) -> Var.of_string v) reals |> Array.of_list in
    let env = Environment.make integers reals in
    Environment.print Format.std_formatter env;
    let a = Abstract1.top man env in
    let domains = List.map (fun (_,v,dom) -> abstract_of_dom a v dom) p.init in
    let abs = List.fold_left (Abstract1.meet man) (Abstract1.top man env) domains in
    let rec aux ((res_all, res_lin, res_nonlin) as res) constraints =
      match constraints with
      | [] -> res
      | h::tl when is_cons_linear h -> 
	let ap_cons = bexpr_to_apron h env in
	aux ((ap_cons::res_all), (ap_cons::res_lin), (res_nonlin)) tl
      | h::tl -> 
	let ap_cons = bexpr_to_apron h env in
	aux ((ap_cons::res_all), (res_lin), (ap_cons::res_nonlin)) tl
    in 
    let all,c_l,c_nl = aux ([],[],[]) p.constraints in
    let (domains:Lincons1.earray) = Abstract1.to_lincons_array man abs      
    in
    (env,
     domains,
     [all],
     (Utils.tcons_list_to_earray all),
     (Utils.tcons_list_to_earray c_nl),
     (Utils.tcons_list_to_earray c_l))
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

  let of_problem p =
    let (env,domains,_,_,_,_) = Translate.to_apron p in
    Abstract1.of_lincons_array man env domains
      
  let is_bottom b = Abstract1.is_bottom man b
    
  let sat_cons b c =
    let env = Abstract1.env b in
    Translate.bexpr_to_apron c env |> Abstract1.sat_tcons man b
	
  let meet b c = 
    let env = Abstract1.env b in
    let c = Translate.bexpr_to_apron c env in
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
end
