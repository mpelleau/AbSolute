open Apron
open Syntax

module type ADomain = sig 
  type t
  val get_manager: t Manager.t 
end

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
    Format.printf "%a\n" Tcons1.print res;
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
    Format.printf "environnement: ";
    Environment.print Format.std_formatter env;
    Format.printf "\n";
    let a = Abstract1.top man env in
    Format.printf "top =%a@\n%!" Abstract1.print a; 
    let domains = List.map (fun (_,v,dom) -> abstract_of_dom a v dom) p.init in
    let abs = List.fold_left (Abstract1.meet man) (Abstract1.top man env) domains in
    Format.printf "domain joined = %a@." Abstract1.print abs;
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
     (Utils.tcons_list_to_earray c_l),
     (Utils.tcons_list_to_earray c_nl))
end
