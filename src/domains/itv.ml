(*
  An abstract fixpoint solver based on Constraint Programming
  
  Author: Antoine Mine
  Copyright 2015
*)

(* 
   Generic intervals.

   Can be instantiated with any bound type.
*)


open Bot
open Bound_sig


module Itv(B:BOUND) = (struct
    
  (************************************************************************)
  (* TYPES *)
  (************************************************************************)


  (* interval bound (possibly -oo or +oo *)
  module B = B
  type bound = B.t

  (* an interval is a pair of bounds (lower,upper);
     intervals are always non-empty: lower <= upper;
     functions that can return an empty interval return it as Bot
   *) 
  type t = bound * bound


  (* not all pairs of rationals are valid intervals *)
  let validate ((l,h):t) : t =
    match B.classify l, B.classify h with
    | B.INVALID,_ | _,B.INVALID  | B.MINF,_ | _,B.INF ->
        invalid_arg "int.validate"
    | _ -> 
        if B.gt l h then invalid_arg "itv.validate" else l,h
          

  (* maps empty intervals to explicit bottom *)
  let check_bot ((l,h):t) : t bot =
    if B.leq l h then Nb (l,h) else Bot
      


  (************************************************************************)
  (* CONSTRUCTORS AND CONSTANTS *)
  (************************************************************************)

      
  let of_bound (x:B.t) : t = validate (x,x)
      
  let zero : t = of_bound B.zero
      
  let one : t = of_bound B.one
      
  let minus_one : t = of_bound B.minus_one
      
  let top : t = B.minus_inf, B.inf
      
  let zero_one : t = B.zero, B.one
      
  let minus_one_zero : t = B.minus_one, B.zero
      
  let minus_one_one : t = B.minus_one, B.one
      
  let positive : t = B.zero, B.inf
      
  let negative : t = B.minus_inf, B.zero

  let of_bounds (l:bound) (h:bound) = validate (l,h)
      
  let of_ints (l:int) (h:int) : t = of_bounds (B.of_int_down l) (B.of_int_up h)
      
  let of_int (x:int) = of_ints x x
      
  (* let of_rats (l:Q.t) (h:Q.t) : t = of_bounds (B.of_rat_down l) (B.of_rat_up h) *)
      
  (* let of_rat (x:Q.t) = of_rats x x *)
      
  let of_floats (l:float) (h:float) : t = of_bounds (B.of_float_down l) (B.of_float_up h)
      
  let of_float (x:float) = of_floats x x
      
  let hull (x:B.t) (y:B.t) = B.min x y, B.max x y



  (************************************************************************)
  (* PRINTING *)
  (************************************************************************)

    (*
  let to_string ((l,h):t) : string = 
    Printf.sprintf "[%s;%s]" (B.to_string l) (B.to_string h)
     *)
      
  let to_string ((l,h):t) : string = 
    Printf.sprintf "[%f;%f]" (B.to_float_down l) (B.to_float_up h)
      
      
  (* printing *)
  let output chan x = output_string chan (to_string x)
  let sprint () x = to_string x
  let bprint b x = Buffer.add_string b (to_string x)
  let pp_print f x = Format.pp_print_string f (to_string x)
      


  (************************************************************************)
  (* SET-THEORETIC *)
  (************************************************************************)


  (* operations *)
  (* ---------- *)


  let join ((l1,h1):t) ((l2,h2):t) : t =
    B.min l1 l2, B.max h1 h2

  (* returns None if the set-union cannot be exactly represented *)
  let union ((l1,h1):t) ((l2,h2):t) : t option =
    if B.leq l1 h2 && B.leq l2 h1 then Some (B.min l1 l2, B.max h1 h2)
    else None
        
  let meet ((l1,h1):t) ((l2,h2):t) : t bot =
    check_bot (B.max l1 l2, B.min h1 h2)
      

  (* predicates *)
  (* ---------- *)


  let equal ((l1,h1):t) ((l2,h2):t) : bool =
    B.equal l1 l2 && B.equal h1 h2
      
  let subseteq ((l1,h1):t) ((l2,h2):t) : bool =
    B.geq l1 l2 && B.leq h1 h2
      
  let contains ((l,h):t) (x:B.t) : bool =
    B.leq l x && B.leq x h
      
  let intersect ((l1,h1):t) ((l2,h2):t) : bool =
    B.leq l1 h2 && B.leq l2 h1


  let is_finite x =
    B.classify x = B.FINITE
      
  let is_bounded ((l,h):t) =
    is_finite l && is_finite h
      
  let is_singleton ((l,h):t) : bool =
    is_finite l && B.equal l h
      

  (* mesure *)
  (* ------ *)


  (* length of the intersection (>= 0) *)
  let overlap ((l1,h1):t) ((l2,h2):t) : B.t  =
    B.max B.zero (B.sub_up (B.min h1 h2) (B.max l1 l2))
      
  let range ((l,h):t) : B.t = 
    B.sub_up h l
      
  let magnitude ((l,h):t) : B.t =
    B.max (B.abs l) (B.abs h)
      


  (* split *)
  (* ----- *)


  (* find the mean of the interval;
     when a bound is infinite, then "mean" is some value strictly inside the
     interval
   *)
  let mean ((l,h):t) : B.t list =
    let res =
    match is_finite l, is_finite h with
    | true,true -> 
      (* finite bounds: returns the actual mean *)
      B.div_up (B.add_up l h) B.two
    | true,false -> 
      (* [l,+oo] *)
      if B.sign l < 0 then B.zero      (* cut at 0 if [l,+oo] contains 0 *)
      else if B.sign l = 0 then B.one  (* cut at 1 if [l,+oo] touches 0 *)
      else B.mul_up l B.two            (* cut at 2l if [l,+oo] is positive *)
    | false,true ->
        (* [-oo,h]: similar to [l,+oo] *)
        if B.sign h > 0 then B.zero
        else if B.sign h = 0 then B.minus_one
        else B.mul_down h B.two
    | false,false -> 
        (* the mean of [-oo,+oo] is 0 *)
      B.zero
    in [res]

  (* splits in two, around m *)
  let split ((l,h):t) (m:bound list) : (t bot) list =
    let to_pair = ref l in
    let list = 
      List.rev_map (fun e -> let res = (!to_pair,e) in to_pair := e; res) m
    in 
    List.rev_map check_bot ((!to_pair,h)::list)

  let split_integer ((l,h):t) (m:bound list) : (t bot) list =
    let to_pair = ref l in
    let list = 
      List.rev_map (fun e ->
	let ll,hh = B.floor e, B.ceil e in
	let res = (!to_pair,ll) in to_pair := hh ; res) 
	m
    in
    List.rev_map check_bot ((!to_pair,h)::list)
   


  (************************************************************************)
  (* INTERVAL ARITHMETICS (FORWARD EVALUATION) *)
  (************************************************************************)


  let neg ((l,h):t) : t =
    B.neg h, B.neg l
      
  let abs ((l,h):t) : t =
    if contains (l,h) B.zero then B.zero, B.max (B.abs l) (B.abs h)
    else hull (B.abs l) (B.abs h)
        
        
  let add ((l1,h1):t) ((l2,h2):t) : t =
    B.add_down l1 l2, B.add_up h1 h2
      
  let sub ((l1,h1):t) ((l2,h2):t) : t =
    B.sub_down l1 h2, B.sub_up h1 l2

      
  (* helper: oo * 0 = 0 when multiplying bounds *)
  let bound_mul f x y =
    if B.sign x = 0 || B.sign y = 0 then B.zero else f x y
      
  let bound_mul_up   = bound_mul B.mul_up
  let bound_mul_down = bound_mul B.mul_down
      
  let mix4 up down ((l1,h1):t) ((l2,h2):t) =
    B.min (B.min (down l1 l2) (down l1 h2)) (B.min (down h1 l2) (down h1 h2)),
    B.max (B.max (up   l1 l2) (up   l1 h2)) (B.max (up   h1 l2) (up   h1 h2))
      
  let mul =
    mix4 bound_mul_up bound_mul_down
      
  (* helper: 0/0 = 0, x/0 = sign(x) oo *)
  let bound_div f x y =
    match B.sign x, B.sign y with
    |  0,_ -> B.zero
    |  1,0 -> B.inf
    | -1,0 -> B.minus_inf
    | _ -> f x y
          
  let bound_div_up   = bound_div B.div_up
  let bound_div_down = bound_div B.div_down
      
  (* helper: assumes i2 has constant sign *) 
  let div_sign =
    mix4 bound_div_up bound_div_down

  (* return valid values (possibly Bot) + possible division by zero *)
  let div (i1:t) (i2:t) : t bot * bool =
    (* split into positive and negative dividends *)
    let pos = (lift_bot (div_sign i1)) (meet i2 positive)
    and neg = (lift_bot (div_sign i1)) (meet i2 negative) in
    (* joins the result *)
    join_bot2 join pos neg,
    contains i2 B.zero
        
  (* interval square root *)
  let sqrt ((l,h):t) : t bot =
    if B.sign h < 0 then Bot else
    let l = B.max l B.zero in
    Nb (B.sqrt_down l, B.sqrt_up h)
      


  (************************************************************************)
  (* FILTERING (TEST TRANSFER FUNCTIONS) *)
  (************************************************************************)


  (* tests *)
  (* ----- *)

  let filter_leq ((l1,h1):t) ((l2,h2):t) : (t*t) bot =
    merge_bot2 (check_bot (l1, B.min h1 h2)) (check_bot (B.max l1 l2, h2))
      
  let filter_geq ((l1,h1):t) ((l2,h2):t) : (t*t) bot =
    merge_bot2 (check_bot (B.max l1 l2, h1)) (check_bot (l2, B.min h1 h2))
      
  let filter_lt ((l1,_) as i1:t) ((l2,_) as i2:t) : (t*t) bot =
    if is_singleton i1 && is_singleton i2 && B.equal l1 l2 then Bot
    else filter_leq i1 i2
        
  let filter_gt ((l1,_) as i1:t) ((l2,_) as i2:t) : (t*t) bot =
    if is_singleton i1 && is_singleton i2 && B.equal l1 l2 then Bot
    else filter_geq i1 i2
        
  let filter_eq (i1:t) (i2:t) : (t*t) bot =
    lift_bot (fun x -> x,x) (meet i1 i2)
      
  let filter_neq ((l1,_) as i1:t) ((l2,_) as i2:t) : (t*t) bot =
    if is_singleton i1 && is_singleton i2 && B.equal l1 l2 then Bot
    else Nb (i1,i2)

  let filter_lt_int ((l1,h1):t) ((l2,h2):t) : (t*t) bot =
    merge_bot2
      (check_bot (l1, B.min h1 (B.sub_up h2 B.one)))
      (check_bot (B.max (B.add_down l1 B.one) l2, h2))

  let filter_gt_int ((l1,h1):t) ((l2,h2):t) : (t*t) bot =
    merge_bot2
      (check_bot (B.max l1 (B.add_down l2 B.one), h1))
      (check_bot (l2, B.min (B.sub_up h1 B.one) h2))
      
  let filter_neq_int ((l1,h1):t) ((l2,h2):t) : (t*t) bot =
    match is_singleton (l1,h1), is_singleton (l2,h2) with
    | true, true when B.equal l1 l2 -> Bot
    | true, false when B.equal l1 l2 ->
        merge_bot2 (Nb (l1,l2)) (check_bot (B.add_down l2 B.one, h2))
    | true, false when B.equal l1 h2 ->
        merge_bot2 (Nb (l1,l2)) (check_bot (l2, B.sub_up h2 B.one))
    | false, true when B.equal l1 l2 ->
        merge_bot2 (check_bot (B.add_down l1 B.one, h1)) (Nb (l2,h2))
    | false, true when B.equal h1 l2 ->
        merge_bot2 (check_bot (l1, B.sub_up h1 B.one)) (Nb (l2,h2))
    | _ -> Nb ((l1,h1),(l2,h2))
    
 
  (* arithmetic *)
  (* --------- *)

  (* r = -i => i = -r *)
  let filter_neg (i:t) (r:t) : t bot =
    meet i (neg r)
      
  let filter_abs ((il,ih) as i:t) ((rl,rh) as r:t) : t bot =
    if B.sign il >= 0 then meet i r
    else if B.sign ih <= 0 then meet i (neg r)
    else meet i (B.neg rh, rh)
        
  (* r = i1+i2 => i1 = r-i1 /\ i2 = r-i1 *)
  let filter_add (i1:t) (i2:t) (r:t) : (t*t) bot =
    merge_bot2 (meet i1 (sub r i2)) (meet i2 (sub r i1))
      
  (* r = i1-i2 => i1 = i2+r /\ i2 = i1-r *)
  let filter_sub (i1:t) (i2:t) (r:t) : (t*t) bot =
    merge_bot2 (meet i1 (add i2 r)) (meet i2 (sub i1 r))

  (* r = i1*i2 => (i1 = r/i2 \/ i2=r=0) /\ (i2 = r/i1 \/ i1=r=0) *)
  let filter_mul (i1:t) (i2:t) (r:t) : (t*t) bot =
    merge_bot2
      (if contains r B.zero && contains i2 B.zero then Nb i1
      else match fst (div r i2) with Bot -> Bot | Nb x -> meet i1 x)
      (if contains r B.zero && contains i1 B.zero then Nb i2
      else match fst (div r i1) with Bot -> Bot | Nb x -> meet i2 x)
      
  (* r = i1/i2 => i1 = i2*r /\ (i2 = i1/r \/ i1=r=0) *)
  let filter_div (i1:t) (i2:t) (r:t) : (t*t) bot =
    merge_bot2
      (meet i1 (mul i2 r))
      (if contains r B.zero && contains i1 B.zero then Nb i2
      else match fst (div i1 r) with Bot -> Bot | Nb x -> meet i2 x)
      
  (* r = sqrt i => i = r*r or i < 0 *)
  let filter_sqrt ((il,ih) as i:t) ((rl,rh):t) : t bot =
    let rr = B.mul_down rl rl, B.mul_up rh rh in
    if B.sign il >= 0 then meet i rr
    else meet i (B.minus_inf, snd rr)

end)
    
module ItvF = Itv(Bound_float)
(* module ItvQ = Itv(Bound_rational) *)
