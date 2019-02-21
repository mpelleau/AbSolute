open Vpl

module Binding_VPL_Apron = struct

    module Coeff = Scalar.Rat

    module I = WrapperTraductors.Interface(Coeff)

    module E = Apron.Texpr1

    module Translate = struct

    	let var : Var.t -> Apron.Var.t
    		= fun v ->
    		Var.to_string v
    		|> Apron.Var.of_string

    	let num : Q.t -> Mpq.t
    		= fun q ->
    		Coeff.to_string q
    		|> Mpq.of_string

    	let constant : Coeff.t -> Apron.Coeff.t
    		= fun c ->
    		num c
    		|> Apron.Coeff.s_of_mpq

    	(* TODO : est-ce ce qu'on veut? *)
    	let rnd = E.Rnd
    	let typ = E.Real

    	module Set = Set.Make (struct type t = Apron.Var.t let compare = Apron.Var.compare end)

    	let rec term' : I.Term.t -> E.expr * Set.t
    		= function
    		| I.Term.Var v ->
    			let v' = var v in
    			(E.Var v', Set.add v' Set.empty)
    		| I.Term.Cte c -> (E.Cst (constant c) , Set.empty)
    		| I.Term.Add (t1,t2) ->
    			let (e1, v1) = term' t1
    			and (e2, v2) = term' t2 in
    			(E.Binop (E.Add, e1, e2, typ, rnd), Set.union v1 v2)
    		| I.Term.Sum l -> List.fold_left
    			(fun (e_res, vs) t ->
    				let (e,v) = term' t in
    				(E.Binop (E.Add, e_res, e, typ, rnd), Set.union v vs))
    			(term' (List.hd l))
    			(List.tl l)
    		| I.Term.Opp t ->
    			let (e,vs) = term' t in
    			(E.Unop (E.Neg, e, typ, rnd), vs)
    		| I.Term.Mul (t1,t2) ->
    			let (e1, v1) = term' t1
    			and (e2, v2) = term' t2 in
    			(E.Binop (E.Mul, e1, e2, typ, rnd), Set.union v1 v2)
    		| I.Term.Prod l -> List.fold_left
    			(fun (e_res, vs) t ->
    				let (e,v) = term' t in
    				(E.Binop (E.Mul, e_res, e, typ, rnd), Set.union v vs))
    			(term' (List.hd l))
    			(List.tl l)
            | I.Term.Div (t1,t2) ->
                let (e1, v1) = term' t1
                and (e2, v2) = term' t2 in
                (E.Binop (E.Div, e1, e2, typ, rnd), Set.union v1 v2)
    		| I.Term.Annot (annot, t) -> term' t
            | I.Term.Poly _ -> Pervasives.failwith "term': poly unimplemented"

    	let term : I.Term.t -> E.expr * (Apron.Var.t list)
    		= fun t ->
    		let (e,vs) = term' t in
    		(e, Set.elements vs)

    	let relation : E.t -> Cstr_type.cmpT_extended -> Apron.Tcons1.t
    		= fun expr -> Apron.Tcons1.(function
    		| Cstr_type.LE ->
    			let expr = E.unop E.Neg expr typ rnd in
    		 	make expr SUPEQ
    		| Cstr_type.LT ->
    			let expr = E.unop E.Neg expr typ rnd in
    		 	make expr SUP
    		| Cstr_type.GE -> make expr SUPEQ
    		| Cstr_type.GT -> make expr SUP
    		| Cstr_type.EQ -> make expr EQ
    		| Cstr_type.NEQ -> make expr DISEQ)

    	let inv_cmp : Cstr_type.cmpT_extended -> Cstr_type.cmpT_extended
    		= function
    		| Cstr_type.LE -> Cstr_type.GT
    		| Cstr_type.LT -> Cstr_type.GE
    		| Cstr_type.GE -> Cstr_type.LT
    		| Cstr_type.GT -> Cstr_type.LE
    		| Cstr_type.EQ -> Cstr_type.NEQ
    		| Cstr_type.NEQ -> Cstr_type.EQ

    	let minkowski_hyper : (Q.t * Var.t* Q.t) -> E.expr * Apron.Var.t
    		= fun (inf, v, sup) ->
    		let itv = Apron.Coeff.i_of_mpq (num inf) (num sup)
    		and v' = var v in
    		(E.Binop (E.Add, E.Var v', E.Cst itv, typ, rnd), v')

    	let add_vars : Apron.Environment.t -> Apron.Var.t list -> Apron.Environment.t
    		= fun env l ->
    		List.fold_left
    			(fun env v ->
    				try Apron.Environment.add env [||] [| v |]
    				with Failure _ -> env)
    			env l

    	(* On suppose qu'il n'y a que des AND *)
    	let rec cond' : bool -> Apron.Environment.t -> I.Cond.t -> (Apron.Tcons1.t list * Apron.Environment.t) option
    		= fun positive env -> function
    		| I.Cond.Basic true -> Some ([], env)
    		| I.Cond.Basic false -> None
          | I.Cond.Atom (t1, cmp, t2) -> begin
          	let t = I.Term.Add(t1, I.Term.Opp t2) in
          	let (expr', vs) = term t in
          	let env = add_vars env vs in
          	let expr' = E.of_expr env expr' in
          	let cmp' = if positive
          		then cmp
          		else inv_cmp cmp
         		in
          	Some ([relation expr' cmp'], env)
          	end
          | I.Cond.BinL (t1, WrapperTraductors.AND, t2) -> begin
          	match (cond' positive env t1, cond' positive env t2) with
          	| None, _ | _, None -> None
          	| Some (l1, env1), Some (l2, env2) -> Some (l1 @ l2, Apron.Environment.lce env1 env2)
          	end
          | I.Cond.BinL (t1, WrapperTraductors.OR, t2) -> Pervasives.invalid_arg "Apron_binding.cond : OR"
          | I.Cond.Not t -> cond' (not positive) env t

    	let cond : Apron.Environment.t -> I.Cond.t -> (Apron.Tcons1.t list * Apron.Environment.t) option
    		= fun env cond ->
    		match cond' true env cond with
    		| None -> None
    		| Some (l, env) -> Some (l, env)
    end

    open Apron

    module Make (Man: sig
    	type t
    	val name : string
    	val manager: t Manager.t
    	end) = struct

    	type t = Man.t Abstract1.t

      	let man = Man.manager

    	let empty_env = Environment.make [||] [||]

    	let top = Abstract1.top man empty_env

      	let update_env : Environment.t -> t -> t
    		= fun env state ->
    		if Environment.compare env (Abstract1.env state) = 0
    		then state
    		else Abstract1.change_environment Man.manager state env false (* TODO: que fait se boolén?*)

    	let update_states state1 state2 =
    		let env1 = Abstract1.env state1
    		and env2 = Abstract1.env state2 in
    		if Environment.equal env1 env2
    		then (state1,state2)
    		else let env' = Environment.lce (Abstract1.env state1) (Abstract1.env state2) in
    			(update_env env' state1,
    			 update_env env' state2)

    	let print : t -> unit
    		= fun state ->
    		Abstract1.print Format.std_formatter state

    	let mk_earray : Environment.t -> I.Cond.t -> Tcons1.earray * Environment.t
    		= fun env cond ->
    		let (l, env') = match Translate.cond env cond with
    			| None -> Pervasives.failwith "Apron_binding.mk_earray"
    			| Some x -> x
    		in
    		let a = ref (Tcons1.array_make env' (List.length l)) in
    		List.iteri
    			(fun i cons ->
    				let cons' = Tcons1.extend_environment cons env' in
    				Tcons1.array_set !a i cons')
    			l;
    		!a, env'

    	let assume : I.Cond.t -> t -> t
    		= fun cond state ->
    		let env = Abstract1.env state in
    		let (a,env) = mk_earray env cond in
    		let state = update_env env state in
    		Abstract1.meet_tcons_array man state a

    	module Cs = Cstr.Rat

    	(* TODO: use Term.of_cstr and Cond.of_cstrs of CWrappers *)
      let cstr_to_term : Cs.t -> (I.Term.t * Cstr_type.cmpT_extended)
    		= fun cstr ->
    		let l = Cs.get_v cstr
    			|> Cs.Vec.toList
    			|> List.map (fun (var,coeff) -> I.Term.Mul (I.Term.Var var, I.Term.Cte coeff))
    		and c = Cs.get_c cstr |> Cs.Vec.Coeff.neg |> fun c -> I.Term.Cte c
        in
        let cmp = match Cs.get_typ cstr with
        | Cstr_type.Le -> Cstr_type.LE
        | Cstr_type.Lt -> Cstr_type.LT
        | Cstr_type.Eq -> Cstr_type.EQ
        in
        (I.Term.Sum (c::l), cmp)

    	let cstrs_to_cond : Cs.t list -> I.Cond.t
    		= fun cstrs ->
    		List.map cstr_to_term cstrs
    		|> List.fold_left
        (fun cond (term,cmp) ->
            let atom = I.Cond.Atom (term, cmp, I.Term.Cte Cs.Vec.Coeff.z) in
    				I.Cond.BinL (cond, WrapperTraductors.AND, atom))
    			(I.Cond.Basic true)

    	let mk : Cs.t list -> t
    		= fun cstrs ->
    		let cond = cstrs_to_cond cstrs in
    		assume cond top

    	let n_cstr : t -> int
    		= fun poly ->
    		(Apron.Abstract1.to_lincons_array man poly).Apron.Lincons1.lincons0_array
    		|> Array.length
    end

    module Apron_Polka_Loose = struct
    	let name = "Polka_loose"
    	type t = Polka.loose Polka.t
    	let manager = Polka.manager_alloc_loose ()
    end

    module Apron_Polka_Strict = struct
    	let name = "Polka_strict"
    	type t = Polka.strict Polka.t
    	let manager = Polka.manager_alloc_strict ()
    end
    module Apron_Polka_Equalities = struct
    	let name = "Polka_equalities"
    	type t = Polka.equalities Polka.t
    	let manager = Polka.manager_alloc_equalities ()
    end

    module Polka_Strict = Make (Apron_Polka_Strict)
end

type t = Vpl_domain.VplCP.t

let is_empty = Vpl_domain.VplCP.is_empty

let to_abs : t * Csp.csts -> t
    = fun (p,csts) ->
    List.fold_left
        (fun p (var, (bi, bs)) ->
            let cond = Csp.And
            (Csp.Cmp (Csp.LEQ, Csp.Var var, Csp.Cst (bs, Csp.Real)),
            Csp.Cmp (Csp.GEQ, Csp.Var var, Csp.Cst (bi, Csp.Real)))
            |> Vpl_domain.VPL.to_cond in
            Vpl_domain.VplCP.assume cond p)
        p csts

let bound : t -> Csp.var -> Mpqf.t * Mpqf.t
    = fun pol var ->
    let term = Csp.Var var in
    let itv = Vpl_domain.VplCP.itvize term pol in
    let low = match itv.Pol.low with
        | Pol.Infty -> Mpqf.of_float min_float
    	| Pol.Open r | Pol.Closed r -> Q.to_string r |> Mpqf.of_string
    and up = match itv.Pol.up with
        | Pol.Infty -> Mpqf.of_float max_float
    	| Pol.Open r | Pol.Closed r -> Q.to_string r |> Mpqf.of_string
    in
    (low,up)

let draw2d : t -> (Csp.var * Csp.var) -> Graphics.color -> unit
    = fun pol (x,y) ->

    let cond = Vpl_domain.VplCP.get_b_expr pol in
    let x' = Vpl_domain.Ident.toVar x |> Var.to_string
    and y' = Vpl_domain.Ident.toVar y |> Var.to_string
    in
    let pol_apron = Binding_VPL_Apron.Polka_Strict.assume cond Binding_VPL_Apron.Polka_Strict.top in
    (*Binding_VPL_Apron.Polka_Strict.print pol_apron;*)
    Apron_drawer.PolyDrawer.draw2d pol_apron (x',y')

let print : Format.formatter -> t -> unit
    = fun _ _ -> ()

let print_latex : Format.formatter -> t -> (Csp.var * Csp.var) -> Graphics.color -> unit
    = fun _ _ _ _ -> ()

let draw3d : Format.formatter -> t list -> (Csp.var * Csp.var * Csp.var) -> unit
    = fun _ _ _ -> ()
