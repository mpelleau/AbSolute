open Bot
open Bound_sig
open Itv_sig
open Syntax
    
(*******************)
(* GENERIC FUNCTOR *)
(*******************)
  
module Box(I:ITV) = (struct


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

  let dummy_bot = Env.add "dummy" (I.of_ints 10 0) Env.empty

  (* boxes split along variables *)
  type split = var
        
      
  (************************************************************************)
  (* PRINTING *)
  (************************************************************************)

  let print fmt a =
    let first = ref true in
    Env.iter
      (fun v (t,i) ->
	Format.fprintf fmt "%s%a %s:%a" 
	  (if !first then "" else " ") 
	  Syntax.print_typ t 
	  v
	  I.print i;
	first := false
      ) a  

  let to_polygon a v1 v2 =
    let (l1,h1),(l2,h2) = Env.find v1 a, Env.find v2 a in
    let l1,h1 = B.to_float_down l1, B.to_float_up h1
    and l2,h2 = B.to_float_down l2, B.to_float_up h2 in
    [l1,l2; h1,l2; h1,h2; l1,h2]

  let points_to_draw a = function
    | None -> to_polygon a (Env.min_binding a |> fst) (Env.max_binding a |> fst)
    | Some (v1,v2) -> to_polygon a v1 v2
        
      
  (************************************************************************)
  (* SET-THEORETIC *)
  (************************************************************************)


  (* NOTE: all binary operations assume that both boxes are defined on
     the same set of variables;
     otherwise, an Invalid_argument exception will be raised
   *)       
          
  (* predicates *)
  (* ---------- *)

  let is_bottom (a:t) = 
    Env.for_all (fun _ v -> I.check_bot v <> Bot.Bot) a
      
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
      
  let is_small (a:t) (f:float) : (bool * split list)=
    let (v,i) = max_range a in
    (B.to_float_up (I.range i) < f),[v]


  (* split *)
  (* ----- *)
  let is_integer var =
    var.[String.length var - 1] = '%'
          
      

  (* split along a specified variable *)
  let split (a:t) (v:var) (b:bound list) : (t bot) list =
    let i = Env.find v a in
    let i_list = (if is_integer v then I.split_integer else I.split) i b in
    List.map (fun e -> lift_bot (fun ii -> Env.add v ii a) e) i_list
    (*lift_bot (fun ii -> Env.add v ii a) i1,
    lift_bot (fun ii -> Env.add v ii a) i2*)

  (* maximal range of variables *)
  let size (a:t) : B.t =
    Env.fold (fun _ i r -> B.max r (I.range i)) a B.zero
      


  (************************************************************************)
  (* ABSTRACT OPERATIONS *)
  (************************************************************************)


  (* initial box: no variable at all *)
  let init : t = Env.empty
      
  let add_var (a:t) (v:var) (i:i) : t = Env.add v i a
      
  let get_var_range (a:t) (v:var) : i =
    try Env.find v a
    with Not_found -> failwith ("variable not found: "^v)

  let set_var_range (a:t) (v:var) (i:i) : t =
    Env.add v i a
         
  let get_variables (a:t) : var list =
    List.rev (Env.fold (fun v _ acc -> v::acc) a [])
  
      
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
     - we raies Bot_found in case the expression only evaluates to error values
     - otherwise, we return only the non-error values
   *)
  let rec eval (a:t) (e:expr) : bexpri = 
    match e with
    | Var v ->
        let r =
          try Env.find v a
          with Not_found -> failwith ("variable not found: "^v)
        in
        BVar v, r
    | Cst c ->
        let r = I.of_float c in
        BCst r, r
    | Unary (o,e1) -> 
        let _,i1 as b1 = eval a e1 in
        let r = match o with
        | NEG -> I.neg i1
	| ABS -> I.abs i1
        | SQRT -> debot (I.sqrt i1)
	| COS | SIN -> failwith "cos and sin not implemented yet"
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
	| POW -> failwith "power not implemented yet"
        in
        BBinary (o,b1,b2), r


  (* returns a box included in its argument, by removing points such that
     the evaluation is not in the interval;
     not all such points are removed, due to interval abstraction;
     iterating eval and refine can lead to better reults (but is more costly);
     can raise Bot_found
   *)
  let rec refine (a:t) (e:bexpr) (x:i) : t =
    match e with
    | BVar v -> 
        (try Env.add v (debot (I.meet x (Env.find v a))) a
        with Not_found -> failwith ("variable not found: "^v))
    | BCst i -> ignore (debot (I.meet x i)); a
    | BUnary (o,(e1,i1)) ->
        let j = match o with
        | NEG -> I.filter_neg i1 x
        | ABS -> I.filter_abs i1 x
        | SQRT -> I.filter_sqrt i1 x
	| COS | SIN -> failwith "cos and sin not implemented yet"
        in
        refine a e1 (debot j)
    | BBinary (o,(e1,i1),(e2,i2)) ->
        let j = match o with
        | ADD -> I.filter_add i1 i2 x
        | SUB -> I.filter_sub i1 i2 x
        | MUL -> I.filter_mul i1 i2 x
        | DIV -> I.filter_div i1 i2 x
	| POW -> failwith "power not implemented yet"
        in
        let j1,j2 = debot j in
        refine (refine a e1 j1) e2 j2
          


  (* assignment transfer function;
     can return Bot, in case the expression only evaluates to error values
   *)
  let assign (a:t) (v:var) (e:expr) : t bot =
    rebot (fun a -> Env.add v (snd (eval a e)) a) a


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
    rebot 
      (fun a ->
        let j1,j2 = debot j in
        refine (refine a b1 j1) b2 j2
      ) a

  let meet (a:t) (b:Syntax.bexpr) : t = 
    let res = 
      match b with
      | Cmp (binop,e1,e2) -> test a e1 binop e2
      | _ -> failwith "niy" 
    in match res with
    | Bot -> dummy_bot
    | Nb e -> e

  let of_problem _ = failwith ""

  let sat_cons _ = failwith ""

end)


    
(*************)
(* INSTANCES *)
(*************)
    
module BoxF = Box(Itv.ItvF)
