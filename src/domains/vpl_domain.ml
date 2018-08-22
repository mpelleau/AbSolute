open Vpl
open UserInterface

(*
https://git.frama-c.com/frama-c/frama-c/blob/save/feature/eva/vpl/src/plugins/value/domains/vpl/vpl_binding.ok.ml
*)

module Debug = DebugTypes.Debug(struct let name = "AbSolute" end)

module VPL_CP_Profile = Profile.Profile(struct let name = "VPL_CP" end)

module Coeff = Scalar.Rat
(*module Domain = CDomain.PedraQWrapper*)
module Domain = NCDomain.NCVPL_Cstr.Q
include MakeInterface(Coeff)

module Expr = struct
    module Ident = UserInterface.Lift_Ident (struct
        type t = string
        let compare = Pervasives.compare
        let to_string s = s
        end)

    type t = Csp.expr

    exception Out_of_Scope

    let rec to_term : t -> Term.t
        = function
        | Csp.Cst (q, _) -> Term.Cte (Mpqf.to_string q |> Q.of_string)
        | Csp.Var var -> Term.Var (Ident.toVar var)
        | Csp.Unary (Csp.NEG, e) -> Term.Opp (to_term e)
        | Csp.Binary (Csp.ADD, e1, e2) -> Term.Add (to_term e1, to_term e2)
        | Csp.Binary (Csp.SUB, e1, e2) -> Term.Add (to_term e1, Term.Opp (to_term e2))
        | Csp.Binary (Csp.MUL, e1, e2) -> Term.Mul (to_term e1, to_term e2)
        | Csp.Binary (Csp.DIV, e1, e2) -> Term.Div (to_term e1, to_term e2)
        | Csp.Binary (Csp.POW, e, Csp.Cst (q, _)) -> begin
            let term = to_term e
            and n = Mpqf.to_string q |> int_of_string
            in
            Term.Prod (List.map (fun _ -> term) (Misc.range 0 n))
            end
        | _ -> Pervasives.raise Out_of_Scope

    let rec of_term : Term.t -> t
        = function
        | Term.Cte q -> Csp.Cst (Q.to_string q |> Mpqf.of_string, Csp.Real)
        | Term.Var var -> Csp.Var (Ident.ofVar var)
    	| Term.Add (t1,t2) -> Csp.Binary (Csp.ADD, of_term t1, of_term t2)
        | Term.Sum tl -> List.fold_left
            (fun res t -> Csp.Binary (Csp.ADD, res, of_term t))
            (Csp.Cst (Mpqf.of_int 0, Csp.Int))
            tl
        | Term.Opp t -> Csp.Unary (Csp.NEG, of_term t)
    	| Term.Mul (t1, t2) -> Csp.Binary (Csp.MUL, of_term t1, of_term t2)
    	| Term.Prod tl -> List.fold_left
            (fun res t -> Csp.Binary (Csp.MUL, res, of_term t))
            (Csp.Cst (Mpqf.of_int 1, Csp.Int))
            tl
    	| Term.Annot (_, t) -> of_term t
        | _ -> Pervasives.invalid_arg "of_term"
    end

module VPL = struct

	include Lift(Domain)(Expr)

	let translate_cmp : Csp.cmpop -> Cstr.cmpT_extended
		= function
        | Csp.EQ -> Cstr.EQ
        | Csp.LEQ -> Cstr.LE
        | Csp.GEQ -> Cstr.GE
        | Csp.NEQ -> Cstr.NEQ
        | Csp.GT -> Cstr.GT
        | Csp.LT -> Cstr.LT

	let translate_cmp' : Cstr.cmpT_extended -> Csp.cmpop
		= function
        | Cstr.EQ -> Csp.EQ
        | Cstr.LE -> Csp.LEQ
        | Cstr.GE -> Csp.GEQ
        | Cstr.NEQ -> Csp.NEQ
        | Cstr.GT -> Csp.GT
        | Cstr.LT -> Csp.LT

    let rec to_cond : Csp.bexpr -> UserCond.t
        = function
        | Csp.Cmp (cmp, e1, e2) -> UserCond.Atom (e1, translate_cmp cmp, e2)
        | Csp.And (e1, e2) -> UserCond.BinL(to_cond e1, WrapperTraductors.AND, to_cond e2)
        | Csp.Or (e1, e2) -> UserCond.BinL(to_cond e1, WrapperTraductors.OR, to_cond e2)
        | Csp.Not e -> UserCond.Not (to_cond e)

end

module VplCP (* : Domain_signature.AbstractCP *)= struct

    include VPL

    let empty : t = top

    let is_empty = is_bottom

    (* bornage d'une expression *)
    let forward_eval : t -> Csp.expr -> (Mpqf.t * Mpqf.t)
        = fun p expr ->
        let itv = User.itvize p expr in
        let low = match itv.Pol.low with
            | Pol.Infty -> Mpqf.of_float Pervasives.neg_infinity
        	| Pol.Open r | Pol.Closed r -> Q.to_string r |> Mpqf.of_string
        in
        let up = match itv.Pol.up with
            | Pol.Infty -> Mpqf.of_float Pervasives.infinity
        	| Pol.Open r | Pol.Closed r -> Q.to_string r |> Mpqf.of_string
        in
        (low,up)

    let add_var : t -> Csp.typ * Csp.var -> t
        = fun p _ -> p

    let vars : t -> Csp.var list
        = fun p ->
        BuiltIn.get_vars p
        |> List.map Expr.Ident.ofVar

    (* returns the bounds of a variable *)
    let var_bounds : t -> Csp.var -> (Mpqf.t * Mpqf.t)
        = fun p var ->
        forward_eval p (Csp.Var var)

    let bound_vars : t -> Csp.csts
        = fun p ->
        BuiltIn.get_vars p
        |> List.map (fun var ->
            let varCsp = Expr.Ident.ofVar var in
            (varCsp, var_bounds p varCsp)
            )
        |> List.filter
            (fun (var,(bi,bs)) -> Mpqf.equal bi bs)

    (* removes an unconstrained variable to the environnement *)
    let rem_var : t -> Csp.var -> t
        = fun p _ -> p

    let volume : t -> float
        = fun p ->
        match size p with
        | None -> max_float
        | Some value -> Scalar.Rat.to_float value

    let is_small : t -> bool
        = fun p ->
        volume p <= !Constant.precision

    (* Note: the last t is the intersection between the two operands *)
    let prune : t -> t -> t list * t
        = fun p1 p2 ->
        (diff p1 p2, meet p1 p2)

    let split : t -> t list
        = fun p ->
        VPL_CP_Profile.start "split";
        let res = split_in_half p in
        VPL_CP_Profile.stop "split";
        res
        (*
        get_regions p
        *)

    (* TODO: can we use this variable? *)
    let split_along : t -> Csp.var -> t list
        = fun _ _ -> Pervasives.failwith "split_along: unimplemented"

    let filter : t -> (Csp.expr * Csp.cmpop * Csp.expr) -> t
        = fun state (e1,cmp,e2) ->
        Debug.log DebugTypes.Title (lazy "Filter");
        let cond = to_cond (Csp.Cmp (cmp, e1, e2)) in
        Debug.log DebugTypes.MInput (lazy (UserCond.to_string cond));
        User.assume cond state

    (* TODO: Should return the variable with the maximal range as well. *)
    let filter_maxvar : t -> (Csp.expr * Csp.cmpop * Csp.expr) -> t * (Csp.var*float)
        = fun _ _ ->
        Pervasives.failwith "filter_maxvar: unimplemented"

    (* TODO: use Format *)
    let print : Format.formatter -> t -> unit
        = fun _ p ->
        print_endline (to_string Expr.Ident.get_string p)

    (* TODO: to define *)
    let spawn : t -> Csp.instance
        = fun _ ->
        Pervasives.failwith "spawn: unimplemented"

    (* TODO: to define *)
    let is_abstraction : t -> Csp.instance -> bool
        = fun _ ->
        Pervasives.failwith "is_abstraction: unimplemented"

    (* Si une variable entière est un singleton *)
    let is_enumerated : t -> bool
        = fun _ ->
        Pervasives.failwith "is_enumerated: unimplemented"

    let to_bexpr: t -> (Csp.expr * Csp.cmpop * Csp.expr) list
        = let csp_true : Csp.expr * Csp.cmpop * Csp.expr
            = Csp.Cst(Mpqf.of_int 0, Csp.Int), Csp.EQ, Csp.Cst(Mpqf.of_int 0, Csp.Int)
        and csp_false : Csp.expr * Csp.cmpop * Csp.expr
            = Csp.Cst(Mpqf.of_int 0, Csp.Int), Csp.NEQ, Csp.Cst(Mpqf.of_int 0, Csp.Int)
        in
        let rec of_bexpr: Cond.t -> (Csp.expr * Csp.cmpop * Csp.expr) list
            = function
            | Cond.Basic true -> [csp_true]
            | Cond.Basic false -> [csp_false]
        	| Cond.Atom (t1, cmp, t2) -> [Expr.of_term t1, translate_cmp' cmp, Expr.of_term t2]
        	| Cond.BinL (t1, Vpl.WrapperTraductors.AND, t2) -> of_bexpr t1 @ of_bexpr t2
            | _ -> Pervasives.invalid_arg "to_bexpr"
        in
        fun p ->
        BuiltIn.get_cond p
        |> of_bexpr

    let rec is_representable : Csp.bexpr -> Adcp_sig.answer
        = let expr_is_representable : Csp.expr -> Adcp_sig.answer
            = fun t ->
            let poly = Expr.to_term t |> Term.to_poly in
            if Polynomial.is_affine poly
            then Adcp_sig.Yes
            else Adcp_sig.Maybe
        in
        let combine : Adcp_sig.answer * Adcp_sig.answer -> Adcp_sig.answer
            = Adcp_sig.(function
            | (No,_) | (_,No) -> No
            | (Maybe,_) | (_,Maybe) -> Maybe
            | _ -> Yes)
        in
        let not : Adcp_sig.answer -> Adcp_sig.answer
            = Adcp_sig.(function
            | No -> Yes
            | Maybe -> Maybe
            | Yes -> No)
        in
        function
        | Csp.Cmp (_, e1, e2) -> combine (expr_is_representable e1, expr_is_representable e2)
        | Csp.And (e1, e2) -> combine (is_representable e1, is_representable e2)
        | Csp.Or (e1, e2) -> combine (Adcp_sig.Maybe, combine (is_representable e1, is_representable e2))
        | Csp.Not e -> not (is_representable e)

end

let setup_flags : unit -> unit
    = fun () ->
    Flags.handelman_timeout := None

let set_lin s =
    match s with
    | "handelman" -> Flags.lin := Flags.Handelman
    | "itv" -> Flags.lin := Flags.Intervalization
    | "both" -> Flags.lin := Flags.Both
    | _ -> "Linearization " ^ s ^ "undefined. Should be among : handelman, itv, both" |> failwith

let enable_debug : unit -> unit
    = fun () ->
    Vpl.Debug.enable();
    Debug.enable DebugTypes.([Title ; MInput ; MOutput ; Normal ; Detail]);
    Handelman.Debug.enable DebugTypes.([Title ; MInput ; MOutput ; Normal ; Detail]);
    HOtypes.Debug.enable DebugTypes.([Title ; MInput ; MOutput ; Normal ; Detail]);
    Pol.Debug.enable DebugTypes.([Title ; MInput ; MOutput ; Normal ; Detail]);
    Vpl.Debug.print_enable();
    Debug.print_enable();
    Debug.set_color(DebugTypes.Cyan);
    Vpl.Debug.set_colors();
    PSplx.Debug.disable()

let start_profile () =
    VPL_CP_Profile.enable();
    VPL_CP_Profile.reset();
    VPL_CP_Profile.start "vpl";
    setup_flags()

let stop_profile () =
    VPL_CP_Profile.stop "vpl"

let report () =
    Vpl.Profile.report() |> print_endline
