open Bot
open Bound_sig
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
  module Env = Mapext.Make(struct type t=var let compare=compare end)

  (* maps each variable to a (non-empty) interval *)
  type t = i Env.t

  let find v a =
    try (Env.find v a, v) with
      Not_found -> (Env.find (v^"%") a, v^"%")


  (************************************************************************)
  (* PRINTING *)
  (************************************************************************)

  let print fmt a =
    let first = ref true in
    Env.iter
      (fun v i ->
	Format.fprintf fmt "%s%s:%a"
	  (if !first then "" else " ")
	  v
	  I.print i;
	first := false
      ) a

  let float_bounds a v =
    let ((l,h), _) = find v a in
    (B.to_float_down l),(B.to_float_up h)

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

  let is_bottom (a:t) =
    Env.exists (fun _ v -> I.check_bot v = Bot.Bot) a

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
        if B.geq (I.range i) (I.range io) then v,i else vo,io
      ) a (Env.min_binding a)

  let is_small (a:t) : bool =
    let (v,i) = max_range a in
    (B.to_float_up (I.range i) <= !Constant.precision)

  let volume (a:t) : float =
    let vol_bound = Env.fold (fun _ x v -> B.mul_down (I.range x) v) a B.one in
    B.to_float_up vol_bound


  (* split *)
  (* ----- *)
  let is_integer var = var.[String.length var - 1] = '%'

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
    let (v,_) = max_range a in
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
    | BUnary of unop * bexpri
    | BBinary of binop * bexpri * bexpri
    | BVar of var
    | BCst of i

  and bexpri = bexpr * i

  (* interval evaluation of an expression;
     returns the interval result but also an expression tree annotated with
     intermediate results (useful for test transfer functions

     errors (e.g. division by zero) return no result, so:
     - we raise Bot_found in case the expression only evaluates to error values
     - otherwise, we return only the non-error values
   *)
  let rec eval (a:t) (e:expr) : bexpri =
    match e with
    | Var v ->
        let (r, n) =
          try find v a
          with Not_found -> failwith ("variable not found: "^v)
        in
        BVar n, r
    | Cst c ->
        let r = I.of_float c in
        BCst r, r
    | Unary (o,e1) ->
        let _,i1 as b1 = eval a e1 in
        let r = match o with
          | NEG -> I.neg i1
	  | ABS -> I.abs i1
          | SQRT -> debot (I.sqrt i1)
	  | COS -> I.cos i1
	  | SIN -> I.sin i1
	  | TAN -> I.tan i1
	  | COT -> I.cot i1
	  | ACOS -> debot (I.acos i1)
	  | ASIN -> debot (I.asin i1)
	  | ATAN -> I.atan i1
	  | ACOT -> I.acot i1
	  | LN -> debot (I.ln i1)
	  | LOG -> debot (I.log i1)
	  | EXP -> I.exp i1
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
	 | NROOT -> debot (I.n_root i1 i2)
	 | MIN -> I.min i1 i2
	 | MAX -> I.max i1 i2
       in
       BBinary (o,b1,b2), r


  (* returns a box included in its argument, by removing points such that
     the evaluation is not in the interval;
     not all such points are removed, due to interval abstraction;
     iterating eval and refine can lead to better reults (but is more costly);
     can raise Bot_found *)
  let rec refine (a:t) (e:bexpr) (x:i) : t =
    match e with
    | BVar v ->
        (try Env.add v (debot (I.meet x (fst (find v a)))) a
        with Not_found -> failwith ("variable not found: "^v))
    | BCst i -> ignore (debot (I.meet x i)); a
    | BUnary (o,(e1,i1)) ->
        let j = match o with
          | NEG -> I.filter_neg i1 x
          | ABS -> I.filter_abs i1 x
          | SQRT -> I.filter_sqrt i1 x
	  | COS -> I.filter_cos i1 x
	  | SIN -> I.filter_sin i1 x
	  | TAN -> I.filter_tan i1 x
	  | COT -> I.filter_cot i1 x
	  | ACOS -> I.filter_acos i1 x
	  | ASIN -> I.filter_asin i1 x
	  | ATAN -> I.filter_atan i1 x
	  | ACOT -> I.filter_acot i1 x
	  | LN -> I.filter_ln i1 x
	  | LOG -> I.filter_log i1 x
	  | EXP -> I.filter_exp i1 x
        in
        refine a e1 (debot j)
    | BBinary (o,(e1,i1),(e2,i2)) ->
        let j = match o with
          | ADD -> I.filter_add i1 i2 x
          | SUB -> I.filter_sub i1 i2 x
          | MUL -> I.filter_mul i1 i2 x
          | DIV -> I.filter_div i1 i2 x
	  | POW -> I.filter_pow i1 i2 x
	  | NROOT -> I.filter_root i1 i2 x
	  | MIN -> I.filter_min i1 i2 x
	  | MAX -> I.filter_max i1 i2 x
        in
        let j1,j2 = debot j in
        refine (refine a e1 j1) e2 j2

  (* test transfer function *)
  let test (a:t) (e1:expr) (o:cmpop) (e2:expr) : t bot =
    let (b1,i1), (b2,i2) = eval a e1, eval a e2 in
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
    | Bot -> raise Bot_found
    | Nb e -> e

  let filterl (a:t) (e1,binop,e2) : t =
    filter a (e1, binop, e2)

  let empty : t = Env.empty

  let add_var abs (typ,var) : t =
    Env.add (if typ = INT then (var^"%") else var) I.top abs

  let is_enumerated a =
    Env.for_all (fun v i -> (is_integer v |> not) || I.is_singleton i) a

  let forward_eval abs cons =
    let (_, bounds) = eval abs cons in
    I.to_float_range bounds
   

  let rec is_applicable abs (e:expr) : bool =
    match e with
    | Var v -> let (var, name) = try find v abs with 
                                   Not_found -> (I.zero, "")
               in
               let res = match name with
                 | "" -> false
                 | _ -> true
               in
               res
    | Cst _ -> true
    | Unary (_, e1) -> is_applicable abs e1
    | Binary (_, e1, e2) -> (is_applicable abs e1) && (is_applicable abs e2)

  let lfilter (a:t) l : t =
    let la = List.filter (fun (e1, op, e2) -> 
                           (is_applicable a e1) && (is_applicable a e2)) l in
    List.fold_left (fun a' e -> filter a' e) a la

end

(*************)
(* INSTANCES *)
(*************)

module BoxF = Box(Itv.ItvF)
module BoxStrict = Box(Newitv.Test)
