open Tools
open Bot
open Itv_sig
open Csp
open Csp_printer

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

  let find v (a:t) = VarMap.find_fail v a

  (* returns true if var is an integer in the given environment *)
  let is_integer var abs = I.to_annot (VarMap.find abs var) = Csp.Int

  let vars abs =
    Env.fold (fun v x acc ->
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
     otherwise, an Invalid_argument exception will be raised *)

  let join (a:t) (b:t) : t =
    let join_opt a b =
      match a,b with
      | Some a, Some b -> Some (I.join a b)
      | _ -> None
    in
    VarMap.merge (fun _ -> join_opt) a b

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

  let is_small (a:t) : bool =
    let (v,i) = max_range a in
    (I.float_size i) <= !Constant.precision

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

  (* u / v = r => u = r * v /\ (v = u/r \/ u=r=0) *)
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
    let refined1 = if j1 = i1 then a else refine a b1 j1 in
    Nb(if j2 = i2 then refined1 else refine refined1 b2 j2)

  let filter (a:t) (e1,binop,e2) : t =
    match test a e1 binop e2 with
    | Bot ->
       Tools.debug 5 "\n%a\n\t%a\n" print_bexpr (Cmp(binop, e1, e2)) print a;
       raise Bot_found
    | Nb e -> e

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
        let ((op1, e1), (op2, e2)) = I.to_expr x in
        match e1, e2 with
        | Cst e1, Cst e2 ->
           acc@[(Var(v), op1, Cst e1); (Var v, op2, Cst e2)]
        | Cst e1, e2 ->
           acc@[(Var(v), op1, Cst e1); (Var v, op2, e2)]
        | e1, Cst e2 ->
           acc@[(Var(v), op1, e1); (Var v, op2, Cst e2)]
        | e1, e2 ->
           acc@[(Var(v), op1, e1); (Var v, op2, e2)]
      ) a []


  (*********************************)
  (* Sanity and checking functions *)
  (*********************************)

  (* returns an randomly (uniformly?) chosen instanciation of the variables *)
  let spawn (a:t) : instance =
    VarMap.fold (fun k itv acc -> VarMap.add k (Mpqf.of_float (I.spawn itv)) acc) a VarMap.empty

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
    let i = Env.find v a in
    let i_list = I.split i in
    List.fold_left (fun acc b ->
        (Env.add v b a)::acc
    ) [] i_list

  let split (a:t) (_:ctrs) : t list =
    let (v,_) = mix_range a in
    Tools.debug 3 "variable split : %s\n%!" v;
    split_along a v

end

(*************)
(* INSTANCES *)
(*************)

module BoxF       = Box(Trigo.Make(Itv.ItvF))
module BoxStrict  = Box(Trigo.Make(Newitv.Test))
module BoxQ       = Box(Trigo.Make(Itv.ItvQ))
module BoxQStrict = Box(Trigo.Make(Newitv.TestQ))
module BoxMix     = Box(Trigo.Make(Itv_mix))
