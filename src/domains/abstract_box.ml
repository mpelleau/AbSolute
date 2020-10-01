open Tools
open Bot
open Itv_sig
open Csp

(*******************)
(* GENERIC FUNCTOR *)
(*******************)

module Box (I:ITV) = struct


  (************************************************************************)
  (* TYPES *)
  (************************************************************************)

  (* interval and bound inheritance *)
  module I = I
  (* module B = I.B *)

  type i = I.t

  (* maps from variables *)
  module Env = Tools.VarMap

  (* maps each variable to a (non-empty) interval *)
  type t = i Env.t

  (* this domain uses the same language than the one defined in Csp.ml *)
  type internal_constr = Csp.comparison


  let[@warning "-27"] internalize ?elem = Fun.id
  let externalize = Fun.id

  let find v (a:t) = VarMap.find_fail v a

  (* returns true if var is an integer in the given environment *)
  let is_integer var abs = I.to_annot (VarMap.find abs var) = Csp.Int

  let vars abs =
    Env.fold (fun v _ acc ->
        let typ = if is_integer abs v then Int else Real in
        (typ, v)::acc
      ) abs []

  let is_representable _ = Kleene.True

  (************************************************************************)
  (* PRINTING *)
  (************************************************************************)

  let print fmt a =
    VarMap.iter (fun v i -> Format.fprintf fmt "%s:%a\n" v I.print i) a

  let float_bounds a v = I.to_float_range (find v a)

  (************************************************************************)
  (* SET-THEORETIC *)
  (************************************************************************)
  (* NOTE: all binary operations assume that both boxes are defined on
     the same set of variables;
     otherwise, an Invalid_argument exception will be raised *)

  let join (a:t) (b:t) : t * bool =
    let join_opt a b =
      match a,b with
      | Some a, Some b -> Some (I.join a b)
      | _ -> None
    in
    (VarMap.merge (fun _ -> join_opt) a b),false

  let meet (a:t) (b:t) : t =
    let meet_opt a b =
      match a,b with
      | Some a, Some b -> Some (debot (I.meet a b))
      | _ -> raise Bot_found
    in
    VarMap.merge (fun _ -> meet_opt) a b


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

  let volume (a:t) : float =
    VarMap.fold (fun _ x v -> (I.float_size x) *. v) a 1.

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


  (************************************************************************)
  (* ABSTRACT OPERATIONS *)
  (************************************************************************)

  (* trees with nodes annotated with evaluation *)
  type bexpr =
    | BFuncall of string * bexpri list
    | BNeg     of bexpri
    | BBinary  of binop * bexpri * bexpri
    | BVar     of var
    | BCst     of i

  and bexpri = bexpr * i

  (* First step of the HC4-revise algorithm: it computes the intervals for each node of the expression.
     For example: given `x + 3` with `x in [1..3]`, then it annotates `+` with `[4..6]`.
     It returns this new annotated expression tree, and the interval of the root node.

     This function is useful for testing transfer functions errors (e.g. division by zero).
     - We raise Bot_found in case the expression only evaluates to error values.
     - Otherwise, we return only the non-error values. *)
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
    | Cst c ->
       let r = I.of_rat c in
       BCst r, r
    | Neg e1 ->
       let _,i1 as b1 = eval a e1 in
       BNeg b1, I.neg i1
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

  (* Second step of the HC4-revise algorithm.  It propagates the
     intervals from the root of the expression tree `e` to the leaves.
     For example: Given `y = x + 3`, `x in [1..3]`, `y in [1..5]`.
     Then after `eval` we know that the node at `+` has the interval
     `[4..6]`.  Therefore we can intersect `y` with `[4..6]` due to
     the equality.  Note that we can call again `eval` to restrain
     further `+`, and then another round of `refine` will restrain `x`
     as well.  We raise `Bot_found` in case of unsatisfiability. *)
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
    | BNeg (e1,i1) -> refine a e1 (debot  (I.filter_neg i1 x))
    | BBinary (o,(e1,i1),(e2,i2)) ->
       let j = match o with
         | ADD -> I.filter_add i1 i2 x
         | SUB -> I.filter_sub i1 i2 x
         | MUL -> I.filter_mul i1 i2 x
         | DIV -> I.filter_div i1 i2 x
	       | POW -> I.filter_pow i1 i2 x
       in
       let j1,j2 = debot j in
       refine (refine a e1 j1) e2 j2

  (* test transfer function.  Apply the evaluation followed by the
     refine step of the HC4-revise algorithm.  It prunes the domain of
     the variables in `a` according to the constraint `e1 o e2`.  *)
  let test (a:t) (e1:expr) (o:cmpop) (e2:expr) : t Consistency.t =
      let (b1,i1), (b2,i2) = eval a e1, eval a e2 in
      let res = match o with
        | LT  -> (I.filter_lt i1 i2)
        | LEQ -> (I.filter_leq i1 i2)
        (* a > b <=> b < a*)
        | GEQ -> Consistency.map swap_pair (I.filter_leq i2 i1)
        | GT  -> Consistency.map swap_pair (I.filter_lt i2 i1)
        | NEQ -> (I.filter_neq i1 i2)
        | EQ  -> Consistency.map (fun x -> x,x) (I.filter_eq i1 i2)
      in
      Consistency.map (fun (j1,j2) -> refine (refine a b1 j1) b2 j2) res

  let filter (a:t) (e1,binop,e2) : t Consistency.t =
    try test a e1 binop e2
    with Bot_found -> Consistency.Unsat

  let empty : t = Env.empty

  let is_empty abs = Env.is_empty abs

  let add_var (abs:t) (typ,var,domain) : t =
    VarMap.add var
      (match typ,domain with
       | Int,Finite(l,u) -> I.of_rats l u
       | Real,Finite(l,u) -> I.of_rats l u
       | _ -> failwith "cartesian.ml : add_var"
      )
      abs

  let var_bounds (abs:t) var =
    let itv = find var abs in
    I.to_rational_range itv

  let rm_var abs var : t =
    Env.remove var abs

  let forward_eval abs cons =
    let (_, bounds) = eval abs cons in
    I.to_rational_range bounds

  let rec is_applicable (abs:t) (e:expr) : bool =
    match e with
    | Var v -> VarMap.mem v abs
    | Cst _ -> true
    | Neg e1 -> is_applicable abs e1
    | Binary (_, e1, e2) -> (is_applicable abs e1) && (is_applicable abs e2)
    | Funcall (_, _) -> false (* check if sound *)

  let to_bexpr (a:t) : Csp.bexpr =
    match VarMap.bindings a with
    | [] -> assert false
    | (v,i)::tl -> List.fold_left (fun acc (v,i) -> And(acc, I.to_bexpr v i))
                     (I.to_bexpr v i) tl

  (** {1 Sanity and checking functions } *)

  (* returns an randomly (uniformly?) chosen instanciation of the variables *)
  let spawn (a:t) : instance =
    VarMap.(fold (fun k itv -> add k (Q.of_float (I.spawn itv))) a empty)

  (* given an abstraction and instance, verifies if the abstraction is implied
     by the instance *)
  let is_abstraction (a:t) (i:instance) =
    VarMap.for_all (fun k value ->
        let value = Mpqf.to_float value in
        let itv = VarMap.find_fail k a in
        I.contains_float itv value
      ) i

  (* split *)
  (* ----- *)

  let split_along (a:t) (v:var) : t list =
    Tools.debug 3 "variable split : %s\n%!" v;
    let i = VarMap.find v a in
    let i_list = I.split i in
    List.fold_left (fun acc b ->
        (VarMap.add v b a)::acc
      ) [] i_list

  let split (a:t) : t list =
    let (v,i) = max_range a in
    if I.float_size i < !Constant.precision then raise Signature.TooSmall
    else split_along a v

  let render x =
    let vars,values = VarMap.bindings x |> List.split in
    Picasso.Drawable.of_ranges vars (List.map I.to_float_range values)
end

(*************)
(* INSTANCES *)
(*************)

module BoxF       = Box(Trigo.Make(Itv.ItvF))
module BoxStrict  = Box(Trigo.Make(Newitv.Test))
module BoxQ       = Box(Trigo.Make(Itv.ItvQ))
module BoxQStrict = Box(Trigo.Make(Newitv.TestQ))
module BoxMix     = Box(Trigo.Make(Itv_mix))
