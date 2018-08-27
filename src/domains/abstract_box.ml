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
  module B = I.B
  type bound = B.t
  type i = I.t

  (* maps from variables *)
  module Env = Tools.VarMap

  (* maps each variable to a (non-empty) interval *)
  type t = i Env.t

  let find v (a:t) = VarMap.find_fail v a

  let is_integer var = var.[String.length var - 1] = '%'

  let vars abs =
    Env.fold (fun v x acc ->
        let (typ, v) = if is_integer v then
                           (INT, String.sub v 0 (String.length v -1))
                         else (REAL, v) in
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
               List.append lexp [(Csp.Var(v), cl, l);
                                 (Csp.Var(v), ch, h)]
             ) abs []

  let to_expr abs vars : (Csp.expr * Csp.cmpop * Csp.expr) list =
    Env.fold (fun v x lexp ->
               if List.exists (fun vn -> String.equal v vn) vars then
                 let ((cl, l), (ch, h)) = I.to_expr x in
                 List.append lexp [(Csp.Var(v), cl, l);
                                   (Csp.Var(v), ch, h)]
               else if List.exists (fun vn -> String.equal v (vn^"%")) vars then
                 let ((cl, l), (ch, h)) = I.to_expr x in
                 let vn = String.sub v 0 ((String.length v)-2) in
                 List.append lexp [(Csp.Var(vn), cl, l);
                                   (Csp.Var(vn), ch, h)]
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

  (* predicates *)
  (* ---------- *)

  let subseteq (a:t) (b:t) : bool =
    Env.for_all2z (fun _ x y -> I.subseteq x y) a b

  (* mesure *)
  (* ------ *)

  (* diameter *)
  let diameter (a:t) : bound =
    Env.fold (fun _ x v -> B.max (I.range x) v) a B.zero

  (* variable with maximal range *)
  let max_range (a:t) : var * i =
    Env.fold
      (fun v i (vo,io) ->
        if B.gt (I.range i) (I.range io) then v,i else vo,io
      ) a (Env.min_binding a)

  (* variable with maximal range if real or with smallest range if integer *)
  let mix_range (a:t) : var * i =
    Env.fold
      (fun v i (vo,io) ->
        if is_integer v then
          let r = I.range i in
          if (B.neq B.zero r) && (B.gt (I.range io) r) then v,i else vo,io
        else
          vo,io
      ) a (max_range a)

  let is_small (a:t) : bool =
    let (v,i) = max_range a in
    (B.to_float_up (I.range i) <= !Constant.precision)

  let volume (a:t) : float =
    let vol_bound = Env.fold (fun _ x v -> B.mul_down (I.range x) v) a B.one in
    B.to_float_up vol_bound


  (* split *)
  (* ----- *)

  let filter_bounds (a:t) : t =
    let b = Env.mapi (fun v i ->
      if is_integer v then
	      match I.filter_bounds i with
	      | Bot -> raise Bot_found
	      | Nb e -> e
      else i) a in
    b

  let to_bot (a:I.t bot Env.t) : t bot =
    let is_bot = Env.exists (fun v i -> is_Bot i) a in
    if is_bot then Bot
    else Nb (Env.map (fun v -> match v with
    | Nb e -> e
    | _ -> failwith "should not occur"
    ) a)

  let filter_bounds_bot (a:t bot) : t bot =
    match a with
    | Bot -> Bot
    | Nb e -> Env.mapi (fun v i ->
      if is_integer v then I.filter_bounds i
      else Nb i
    ) e
    |> to_bot

let split_along (a:t) (v:var) : t list =
    let i = Env.find v a in
    let i_list =
      if is_integer v then I.split_integer i (I.mean i)
      else I.split i (I.mean i)
    in
    List.fold_left (fun acc b ->
      match b with
      | Nb e -> (Env.add v e a)::acc
      | Bot -> acc
    ) [] i_list

  let split (a:t) : t list =
    let (v,_) = mix_range a in
    (if !Constant.debug > 2 then
      Format.printf " ---- splits along %s ---- \n" v);
    split_along a v

  let prune (a:t) (b:t) : t list * t =
    let rec aux a good = function
      | [] -> good,a
      | (v, i_b)::tl ->
	       let add = fun i -> (Env.add v i a) in
	       let i_a = Env.find v a in
	       let sures,unsure = I.prune i_a i_b in
	       aux (add unsure) (List.rev_append (List.rev_map add sures) good) tl
    in aux a [] (Env.bindings b)


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

  let rec print_bexpri fmt (b, i) =
    match b with
    | BUnary (u, bi) ->
      Format.fprintf fmt "(%a %a) -> %a" print_unop u print_bexpri bi I.print i
    | BBinary (op, e1, e2) ->
      Format.fprintf fmt "(%a %a %a) -> %a" print_bexpri e1 print_binop op print_bexpri e2 I.print i
    | BVar v -> Format.fprintf fmt "%s" v
    | BCst c -> Format.fprintf fmt "cst %a" I.print c
    | BFuncall (f, l) -> Format.fprintf fmt "%s (" f;
                         List.iter (print_bexpri fmt) l;
                         Format.fprintf fmt ") -> %a" I.print i

  (* interval evaluation of an expression;
     returns the interval result but also an expression tree annotated with
     intermediate results (useful for test transfer functions

     errors (e.g. division by zero) return no result, so:
     - we raise Bot_found in case the expression only evaluates to error values
     - otherwise, we return only the non-error values
   *)
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
    | Cst (c,_) ->
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
         | DIV -> debot (fst (I.div i1 i2))
         | MUL ->
            let r = I.mul i1 i2 in
            if e1=e2 then
              (* special case: squares are positive *)
              debot (I.meet r I.positive)
            else r
	 | POW -> I.pow i1 i2
       in
       BBinary (o,b1,b2), r

  (* refines binary operator to handle constants *)
  let refine_bop f1 f2 (e1,i1) (e2,i2) x (b:bool) =
    match e1, e2, b with
    | BCst c1, BCst c2, _ -> Nb (i1, i2)
    | BCst c, _, true -> merge_bot2 (Nb (i1)) (f2 i2 i1 x)
    | BCst c, _, false -> merge_bot2 (Nb (i1)) (f2 i2 x i1)
    | _, BCst c, _ -> merge_bot2 (f1 i1 i2 x) (Nb (i2))
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

  (* returns a box included in its argument, by removing points such that
     the evaluation is not in the interval;
     not all such points are removed, due to interval abstraction;
     iterating eval and refine can lead to better reults (but is more costly);
     can raise Bot_found *)
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

  (* test transfer function *)
  let test (a:t) (e1:expr) (o:cmpop) (e2:expr) : t bot =
    let (b1,i1), (b2,i2) = eval a e1, eval a e2 in
    (*Format.printf "%a %a %a\n" print_bexpri (b1, i1) print_cmpop o print_bexpri (b2, i2);*)
    let j = match o with
      | EQ -> I.filter_eq i1 i2
      | LEQ -> I.filter_leq i1 i2
      | GEQ -> I.filter_geq i1 i2
      | NEQ -> I.filter_neq i1 i2
      (*| NEQ_INT -> I.filter_neq_int i1 i2*)
      | GT -> I.filter_gt i1 i2
      (*| GT_INT -> I.filter_gt_int i1 i2*)
      | LT -> I.filter_lt i1 i2
      (*| LT_INT -> I.filter_lt_int i1 i2*)
    in
    let aux = rebot
      (fun a ->
        let j1,j2 = debot j in
        refine (refine a b1 j1) b2 j2
      ) a
    in
    filter_bounds_bot aux

  let filter (a:t) (e1,binop,e2) : t =
    match test a e1 binop e2 with
    | Bot ->
       if !Constant.debug > 5 then
         Format.printf "\n%a\n\t%a\n" print_bexpr (Cmp(binop, e1, e2)) print a ;
       raise Bot_found
    | Nb e -> e

  let empty : t = Env.empty

  let is_empty abs = Env.is_empty abs

  let add_var abs (typ,var) : t =
    Env.add var I.top abs

  let var_bounds abs var =
    let itv = find var abs in
    I.to_rational_range itv

  let bound_vars abs =
    let b = Env.bindings abs in
    let l = List.filter (fun (v, d) -> I.is_singleton d) b in
    List.map (fun (v, d) -> (v, I.to_rational_range d)) l

  let rem_var abs var : t =
    Env.remove var abs

  let is_enumerated a =
    Env.for_all (fun v i -> (is_integer v |> not) || I.is_singleton i) a

  let forward_eval abs cons =
    let (_, bounds) = eval abs cons in
    I.to_rational_range bounds


  let rec is_applicable abs (e:expr) : bool =
    match e with
    | Var v ->
       (try VarMap.find v abs; true
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
        let (annot, v) = if is_integer v then (Int, String.sub v 0 (String.length v -1)) else (Real, v) in
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
    VarMap.fold (fun k itv acc -> VarMap.add k (Mpqf.of_float (I.spawn itv)) acc) a VarMap.empty

  (* given an abstraction and instance, verifies if the abstraction is implied
     by the instance *)
  let is_abstraction (a:t) (i:instance) =
    VarMap.for_all (fun k value ->
        let value = Mpqf.to_float value in
        let itv = VarMap.find k a in
        let a,b = I.to_float_range itv in
        a <= value && b <= value
      ) i

end

(*************)
(* INSTANCES *)
(*************)

module BoxF = Box(Itv.ItvF)
module BoxStrict = Box(Newitv.Test)
module BoxQ = Box(Itv.ItvQ)
module BoxQStrict = Box(Newitv.TestQ)
