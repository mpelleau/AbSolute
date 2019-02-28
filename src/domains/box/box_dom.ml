open Csp
open Var_store
open Abstract_domain
open Hc4
open Bot

module type Box_sig =
sig
  type t
  type bound
  module I: Itv_sig.ITV with type bound = bound
  type itv = I.t

  val init: var list -> t
  val volume: t -> float
  val get: t -> var -> itv
  val project: (var -> bool) -> t -> t
  val meet_var: var -> itv -> t -> t
  val closure: t -> bconstraint -> t
  val entailment: t -> bconstraint -> kleene
end

module Make
  (B: Bound_sig.BOUND)
  (I: Itv_sig.ITV with type bound=B.t)
  (Store: Var_store_sig with type cell=I.t)
  (Closure: Box_closure_sig with module Store=Store) =
struct
  module I = I
  type itv = I.t
  type bound = B.t
  type t = Store.t

  let init vars =
    List.fold_left (fun box name -> Store.add name I.top box) Store.empty vars

  let get box v = Store.find v box
  let project f box = Store.filter (fun v _ -> f v) box
  let volume box = Store.fold (fun _ x v -> (I.float_size x) *. v) box 1.
  let meet_var var value box = Store.add var (debot (I.meet (Store.find var box) value)) box

  (** Reexported functions from the parametrized modules. *)
  let closure = Closure.closure
  let entailment = Closure.entailment
end

module ItvZ = Trigo.Make(Itv.ItvI)
module ItvQ = Trigo.Make(Itv.ItvQ)
module ItvF = Trigo.Make(Itv.ItvF)

module StoreZ = Var_store.Make(ItvZ)
module StoreQ = Var_store.Make(ItvQ)
module StoreF = Var_store.Make(ItvF)

module BoxZ = Make
  (Bound_int)
  (ItvZ)
  (StoreZ)
  (Hc4.Make(ItvZ)(StoreZ))

module BoxQ = Make
  (Bound_rat)
  (ItvQ)
  (StoreQ)
  (Hc4.Make(ItvQ)(StoreQ))

module BoxF = Make
  (Bound_float)
  (ItvF)
  (StoreF)
  (Hc4.Make(ItvF)(StoreF))

(*

  (* returns true if var is an integer in the given environment *)
  let is_integer abs var = I.to_annot (VarMap.find var abs) = Csp.Int

  let vars abs =
    Env.fold (fun v x acc ->
        let typ = if is_integer abs v then Int else Real in
        (typ, v)::acc
      ) abs []

  let is_representable _ = Adcp_sig.Yes

  (************************************************************************)
  (* PRINTING *)
  (************************************************************************)

  let print fmt a =
    VarMap.iter (fun v i -> Format.fprintf fmt "%s:%a\n" v I.print i) a

  let float_bounds a v = I.to_float_range (find v a)

  let to_expr abs =
    Env.fold (fun v x lexp ->
        let ((cl, l), (ch, h)) = I.to_expr x in
        (Csp.Var(v), cl, l)::(Csp.Var(v), ch, h)::lexp
      ) abs []

  let to_expr abs vars : (Csp.expr * Csp.cmpop * Csp.expr) list =
    Env.fold (fun v x lexp ->
        if List.exists (fun vn -> String.equal v vn) vars then
          let ((cl, l), (ch, h)) = I.to_expr x in
          (Csp.Var(v), cl, l)::(Csp.Var(v), ch, h)::lexp
        else lexp
      ) abs []

  (************************************************************************)
  (* SET-THEORETIC *)
  (************************************************************************)
  (* NOTE: all binary operations assume that both boxes are defined on
     the same set of variables;
     otherwise, an Invalid_argument exception will be raised
   *)

  let join (a:t) (b:t) : t =
    Env.map2z (fun _ x y -> I.join x y) a b

  let meet (a:t) (b:t) : t =
    Env.map2z (fun _ x y -> debot(I.meet x y)) a b

  let meet_var (a:t) (v:var) (value:i) : t =
    Env.add v (debot (I.meet (Env.find v a) value)) a

  (* predicates *)
  (* ---------- *)

  (* mesure *)
  (* ------ *)

  (* variable with maximal range *)
  let max_range (a:t) : var * I.t =
    VarMap.fold
      (fun v i (vo,io) ->
        if (I.float_size i) > (I.float_size io) then v,i else vo,io
      ) a (VarMap.min_binding a)

  (* variable with maximal range if real or with minimal if integer *)
  let mix_range (a:t) : var * I.t =
    VarMap.fold
      (fun v i (vo,io) ->
        if (I.score i) > (I.score io) then v,i else vo,io
      ) a (VarMap.min_binding a)

  let is_small (a:t) : bool =
    let (v,i) = max_range a in
    (I.float_size i) <= !Constant.precision

  (************************)
  (* splitting strategies *)
  (************************)

  let choose a = mix_range a

  let prune =
    match I.prune with
    | None -> None
    | Some itv_diff ->
       Some(fun (a:t) (b:t) ->
           let rec aux a good = function
             | [] -> good
             | (v, i_b)::tl ->
	              let add = fun i -> (Env.add v i a) in
	              let i_a = Env.find v a in
	              let sures = itv_diff i_a i_b in
                let rest = I.meet i_a i_b in
                match rest with
                | Bot -> assert false
                | Nb rest ->
	                 aux (add rest) (List.rev_append (List.rev_map add sures) good) tl
           in aux a [] (Env.bindings b))

  let empty : t = Env.empty

  let is_empty abs = Env.is_empty abs

  let add_var (abs:t) (typ,var) : t =
    Env.add var
      (match typ with
      | Int -> I.top_int
      | Real -> I.top_real)
      abs

  let var_bounds (abs:t) var =
    let itv = find var abs in
    I.to_rational_range itv

  let bound_vars (abs:t) =
    let b = Env.bindings abs in
    let l = List.filter (fun (v, d) -> I.is_singleton d) b in
    List.map (fun (v, d) -> (v, I.to_rational_range d)) l

  let rem_var abs var : t =
    Env.remove var abs

  let forward_eval abs cons =
    let (_, bounds) = eval abs cons in
    I.to_rational_range bounds

  let rec is_applicable (abs:t) (e:expr) : bool =
    match e with
    | Var v ->
       (try ignore (VarMap.find v abs); true
       with Not_found -> false)
    | Cst _ -> true
    | Unary (_, e1) -> is_applicable abs e1
    | Binary (_, e1, e2) -> (is_applicable abs e1) && (is_applicable abs e2)
    | Funcall (name, args) -> false (* check if sound *)

  let lfilter (a:t) l : t =
    let la = List.filter (fun (e1, op, e2) ->
                           (is_applicable a e1) && (is_applicable a e2)) l in
    List.fold_left (fun a' e -> filter a' e) a la

 let to_bexpr (a:t) : (expr * cmpop * expr) list =
    Env.fold (fun v x acc ->
        let annot = if is_integer a v then Int else Real in
        let ((op1, e1), (op2, e2)) = I.to_expr x in
        match e1, e2 with
        | Cst(e1, _), Cst(e2, _) ->
           acc@[(Var(v), op1, Cst(e1, annot)); (Var(v), op2, Cst(e2, annot))]
        | Cst(e1, _), e2 ->
           acc@[(Var(v), op1, Cst(e1, annot)); (Var(v), op2, e2)]
        | e1, Cst(e2, _) ->
           acc@[(Var(v), op1, e1); (Var(v), op2, Cst(e2, annot))]
        | e1, e2 ->
           acc@[(Var(v), op1, e1); (Var(v), op2, e2)]
      ) a []


  (*********************************)
  (* Sanity and checking functions *)
  (*********************************)

  (* returns an randomly (uniformly?) chosen instanciation of the variables *)
  let spawn (a:t) : instance =
    VarMap.fold (fun k itv acc -> VarMap.add k (Bound_rat.of_float (I.spawn itv)) acc) a VarMap.empty

  (* given an abstraction and instance, verifies if the abstraction is implied
     by the instance *)
  let is_abstraction (a:t) (i:instance) =
    VarMap.for_all (fun k value ->
        let value = Bound_rat.to_float_up value in
        let itv = VarMap.find_fail k a in
        I.contains_float itv value
      ) i

  (* split *)
  (* ----- *)

  let split_along (a:t) (v:var) : t list =
    let i = Env.find v a in
    let i_list = I.split i in
    List.fold_left (fun acc b ->
        (Env.add v b a)::acc
    ) [] i_list

  let split (a:t) (_:ctrs) : t list =
    let (v,_) = mix_range a in
    Tools.debug 3 "variable split : %s\n%!" v;
    split_along a v

  let split_on (a:t) (_:ctrs) (xs : instance) : t list =
    let split_on_one (a:t) ((v,value) : (var * Bound_rat.t)) : t list =
      let i = Env.find v a in
      I.split_on i value
      |> List.fold_left (fun acc b ->
          (Env.add v b a)::acc
      ) []
    in
    VarMap.bindings xs
    |> List.fold_left (fun box_list (var,value) ->
      List.fold_left (fun acc box ->
        split_on_one box (var,value) @ acc
      ) [] box_list
    ) [a]

  let shrink (a:t) (c:Bound_rat.t) : t =
    Env.fold (fun var itv (b,acc) ->
      if b && is_empty acc
      then (b,acc)
      else
        match I.shrink itv c with
          | Bot.Bot -> (true, Env.empty)
          | Bot.Nb itv' -> (true, Env.add var itv' acc)
    ) a (false,Env.empty)
    |> Pervasives.snd
 *)
