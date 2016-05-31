(* 
   Generic intervals.

   Can be instantiated with any bound type.
*)


open Bot
open Bound_sig
open Itv_sig


module Union_Itv(I:ITV) = (struct

  include I
  module I = I

  let check_bot2 (l:t bot list) : t list bot =
    let no_bot = List.filter (fun i -> not (is_Bot i)) l in
    if List.length no_bot = 0 then Bot
    else Nb (List.map (fun i -> debot i) no_bot)

  let check_bot3 (l:t list) : t list bot =
    let no_bot = List.filter (fun (il,ih) -> B.leq il ih) l in
    if List.length no_bot = 0 then Bot
    else Nb no_bot

  let rec enumerate l min max =
    if min = max then
      (max::l)
    else
      enumerate (min::l) (min+1) max

  let remove_bot (l:t list bot) : t list =
    match l with
    | Bot -> []
    | Nb (l) -> l

  let remove_bot2 (l:t bot list) : t list =
    let no_bot = List.filter (fun i -> not (is_Bot i)) l in
    if List.length no_bot = 0 then []
    else List.map (fun i -> debot i) no_bot

  (************************************************************************)
  (* SET-THEORETIC *)
  (************************************************************************)

  (* operations *)
  (* ---------- *)

  let join_il (i:t) (l:t list) : t list =
    if List.length l = 0 then [i]
    else 
      let (inter, not_inter) = List.partition (fun e -> I.intersect i e) l in
      if List.length inter = 0 then
	(i::l)
      else
	let final_itv = List.fold_left (fun a b -> I.join a b) i inter in
	(final_itv::not_inter)
	

  let join (l1:t list) (l2:t list) : t list =
    if List.length l1 = 1 && List.length l2 = 1 then
      [I.join (List.hd l1) (List.hd l2)]
    else 
      List.fold_left (fun a b -> join_il b a) l2 l1

  (* returns None if the set-union cannot be exactly represented *)
  let union (l1:t list) (l2:t list) : t list option =
    if List.exists (fun i1 -> List.exists (fun i2 -> I.intersect i1 i2) l2) l1 
    then None
    else Some (join l1 l2)
        
  let meet_il (i:t) (l:t list bot) : t list bot =
    match l with
    | Bot -> Bot
    | Nb l -> 
       let (inter, not_inter) = List.partition (fun e -> I.intersect i e) l in
       if List.length inter = 0 then
	 Bot
       else
	 check_bot2 (List.map (fun a -> I.meet a i) inter)

  let meet (l1:t list) (l2:t list) : t list bot =
    if List.length l1 = 1 && List.length l2 = 1 then
      check_bot2 [I.meet (List.hd l1) (List.hd l2)]
    else 
      List.fold_left (fun a b -> meet_il b a) (Nb l2) l1

  let min_max (l:t list) : bound * bound =
    List.fold_left (fun (min, max) (lv,lh) -> 
		    match (B.leq lv min, B.geq lh max) with
		    | (true, true) -> (lv, lh)
		    | (true, false) -> (lv, max)
		    | (false, true) -> (min, lh)
		    | (false, false) -> (min, max)
		   ) (List.hd l) l

  let to_hull (l:t list) : t = min_max l


  (* predicates *)
  (* ---------- *)

  let equal (l1:t list) (l2:t list) : bool =
    List.length l1 = List.length l2 && List.for_all (fun i1 -> List.exists (fun i2 -> I.equal i1 i2) l2) l1
      
  let subseteq (l1:t list) (l2:t list) : bool =
    List.for_all (fun i1 -> List.exists (fun i2 -> I.subseteq i1 i2) l2) l1
      
  let contains (l:t list) (x:bound) : bool =
    List.exists (fun i -> I.contains i x) l
      
  let intersect (l1:t list) (l2:t list) : bool =
    List.exists (fun i1 -> List.exists (fun i2 -> I.intersect i1 i2) l2) l1
      
  let is_bounded (l:t list) =
    List.for_all (fun i -> I.is_bounded i) l
      
  let is_singleton (l:t list) : bool =
    List.for_all (fun i -> I.is_singleton i) l   

  let simplify (l:t list) : t list =
    let rec aux n l =
      if n > List.length l then
	l
      else
	let i = List.nth l n in
	let r = join_il i l in
	if equal r l then
	  aux (n+1) l
	else
	  aux 0 r
    in
    aux 0 l

  (* mesure *)
  (* ------ *

  let overlap_i (i:t) (l:t list) : bound =
    let inter = List.filter (fun v -> I.intersect v i) l in
    List.fold_left (fun s v -> B.add_up s (I.overlap v i)) B.zero inter

  (* length of the intersection (>= 0) *)
  let overlap (l1:t list) (l2:t list) : bound  =
    if List.length l1 = 1 && List.length l2 = 1 then 
      I.overlap (List.hd l1) (List.hd l2)
    else
      List.fold_left (fun s v -> B.add_up s (overlap_i v l2)) B.zero l1

  let range (l:t list) : bound = 
    let (min,max) = min_max l in
    B.sub_up max min
      
  let magnitude (l:t list) : bound =
    let (min,max) = min_max l in
    B.max (B.abs min) (B.abs max)
      
    *)
  (* split *)
  (* ----- *)

  (* find the mean of the interval;
     when a bound is infinite, then "mean" is some value strictly inside the
     interval
   *)
  let mean_list (l:t list) : bound list =
    let itv = to_hull l in
    I.mean itv

  let split_i ((l,h):t) (b:bound) : t list =
    [(l,b);(b,h)]

  let split_int ((l,h):t) (b:bound) : t list =
    let ll, hh = B.floor b, B.ceil b in
    if ll = hh then [(l,ll);((B.add_up hh B.one),h)]
    else [(l,ll);(hh,h)]

  let split_b f (l:t list) (b:bound) : t list =
    let (inter, not_inter) = List.partition (fun i -> I.contains i b) l in
    let new_itv = List.fold_left (fun ls i -> List.append ls (f i b)) [] inter in
    List.append new_itv not_inter

  (* splits in two, around m *)
  let split_list (l:t list) (m:bound list) : t list bot =
    if List.length l = 1 then
      check_bot2 (I.split (List.hd l) m)
    else
      check_bot3 (List.fold_left (fun ls b -> split_b split_i ls b) l m)

  let split_list_integer (l:t list) (m:bound list) : t list bot =
    if List.length l = 1 then
      check_bot2 (I.split_integer (List.hd l) m)
    else
      check_bot3 (List.fold_left (fun ls b -> split_b split_int ls b) l m)


  (************************************************************************)
  (* FILTERING (TEST TRANSFER FUNCTIONS) *)
  (************************************************************************)

  let apply f (l:t list) : t list =
    if List.length l = 1 then
      [f (List.hd l)]
    else
      List.fold_left (fun lres i -> (f i::lres)) [] l

  let apply_il f (i:t) (l:t list) : t list =
    if List.length l = 1 then
      [f i (List.hd l)]
    else
      List.fold_left (fun lres il -> (f i il::lres)) [] l

  let apply_ll f (l1:t list) (l2:t list) : t list =
    let res = List.fold_left (fun a b -> List.append a (apply_il f b l2)) [] l1 in
    List.filter (fun (il, ih) -> B.geq il ih) res

  let apply_il2 f (i:t) (l:t list) : t bot list =
    if List.length l = 1 then
      [f i (List.hd l)]
    else
      List.fold_left (fun lres il -> (f i il::lres)) [] l

  let apply_ll2 f (l1:t list) (l2:t list) : t list =
    let res = List.fold_left (fun a b -> List.append a (apply_il2 f b l2)) [] l1 in
    let tmp = List.filter (fun i -> not(is_Bot i)) res in
    List.map (fun i -> debot i) tmp

  let div_il (i:t) (l:t list) : t list bot =
    if List.length l = 1 then
      div2 i (List.hd l)
    else
      let res = List.fold_left (fun lres il -> List.append 
						 lres 
						 (match (div2 i il) with
						  | Bot -> []
						  | Nb list -> list
						 )
			       ) [] l 
      in
      if List.length res = 0 then Bot else Nb res

  let div_ll (l1:t list) (l2:t list) : t list bot =
    let res = List.fold_left (fun a b -> List.append 
					   a 
					   (match (div_il b l2) with
					    | Bot -> []
					    | Nb list -> list
					   )
			     ) [] l1 in
    if List.length res = 0 then Bot
    else Nb res
 
  (* arithmetic *)
  (* ---------- *)

  (* r = -i => i = -r *)
  let filter_neg (i:t list) (r:t list) : t list bot =
    meet i (apply neg r)

  let filter_abs (i:t list) (r:t list) : t list bot =
    let aux ((l,h):t) : t list =
      if B.leq l B.zero && B.geq h B.zero then [(B.neg h, h)]
      else if B.gt l B.zero && B.gt h B.zero then [(B.neg h,B.neg l);(l,h)]
      else []
    in
    let r' = List.flatten (List.map (fun a -> aux a) r) in
    meet i r'
        
  (* r = i1+i2 => i1 = r-i2 /\ i2 = r-i1 *)
  let filter_add (i1:t list) (i2:t list) (r:t list) : (t list * t list) bot =
    merge_bot2 (meet i1 (apply_ll sub r i2)) (meet i2 (apply_ll sub r i1))

  (* r = i1-i2 => i1 = i2+r /\ i2 = i1-r *)
  let filter_sub (i1:t list) (i2:t list) (r:t list) : (t list * t list) bot =
    merge_bot2 (meet i1 (apply_ll add i2 r)) (meet i2 (apply_ll sub i1 r))

  (* r = i1*i2 => (i1 = r/i2 \/ i2=r=0) /\ (i2 = r/i1 \/ i1=r=0) *)
  let filter_mul (i1:t list) (i2:t list) (r:t list) : (t list * t list) bot =
    merge_bot2
      (if contains r B.zero && contains i2 B.zero then Nb i1
      else match (div_ll r i2) with Bot -> Bot | Nb x -> meet i1 x)
      (if contains r B.zero && contains i1 B.zero then Nb i2
      else match (div_ll r i1) with Bot -> Bot | Nb x -> meet i2 x)

  (* r = i1/i2 => i1 = i2*r /\ (i2 = i1/r \/ i1=r=0) *)
  let filter_div (i1:t list) (i2:t list) (r:t list) : (t list * t list) bot =
    merge_bot2
      (meet i1 (apply_ll mul i2 r))
      (if contains r B.zero && contains i1 B.zero then Nb i2
      else match (div_ll i1 r) with Bot -> Bot | Nb x -> meet i2 x)

  (* r = sqrt i => i = r*r or i < 0 *)
  let filter_sqrt (i:t list) (r:t list) : t list bot =
    let r2 = List.map (fun itv -> I.pow itv (I.of_int 2)) r in
    let tmp = meet i r2 in
    match tmp with
    | Bot -> Bot
    | Nb l -> Nb (List.map (fun (il,ih) -> if B.sign il < 0 then (B.minus_inf, ih) else (il,ih)) l)


  (*the two closest floating boundaries of pi*)
  let pi_up = B.of_float_up 3.14159265358979356
  let pi_down = B.of_float_down 3.14159265358979312

  (* it improves soundness to use those *)
  let i_pi:t= pi_up, pi_down
  let i_pi_half = div i_pi (of_int 2) |> fst |> debot
  let i_two_pi = add i_pi i_pi
  let i_three_half_of_pi = div (add i_two_pi i_pi) (of_int 2) |> fst |> debot

  let compute_itv (itv:t) (itv':t) i i' =
    let aux = 
      if i mod 2 = 0 then
        add itv' (mul i_pi (of_int i))
      else
        sub (mul i_pi (of_int i')) itv'
    in
    I.meet itv aux

  let applyf f (l1:t list) (l2:t list) : t list bot =
    let res = List.fold_left 
		(fun acc itv -> List.append acc 
					    (List.fold_left 
					       (fun acci ri -> 
						List.append acci 
						  (f itv ri))
					    [] l2)
		) [] l1 in
    if List.length res = 0 then Bot
    else Nb (res)

  let filter_sin_itv (i:t) (r:t) : t list =
    let asin_r = asin r in
    let (aux, _) = div (add i i_pi_half) i_pi in
    match (aux, asin_r) with
    | Bot, _ | _, Bot -> []
    | Nb (p1,p2), Nb (a_r) -> 
      let i1 = int_of_float (B.to_float_up (B.floor p1)) in
      let i2 = int_of_float (B.to_float_up (B.floor p2)) in
      let values = enumerate [] i1 i2 in
      List.map (fun v -> compute_itv i a_r v v) values |> remove_bot2

  (* r = sin i => i = arcsin r *)
  let filter_sin (i:t list) (r:t list) : t list bot =
    applyf filter_sin_itv i r

  let filter_cos_itv (i:t) (r:t) : t list =
    let acos_r = acos r in
    let (aux, _) = div i i_pi in
    match (aux, acos_r) with
    | Bot, _ | _, Bot -> []
    | Nb (p1,p2), Nb (a_r) -> 
      let i1 = int_of_float (B.to_float_up (B.floor p1)) in
      let i2 = int_of_float (B.to_float_up (B.floor p2)) in
      let values = enumerate [] i1 i2 in
      List.map (fun v -> compute_itv i a_r v (v+1)) values |> remove_bot2

  (* r = cos i => i = arccos r *)
  let filter_cos (i:t list) (r:t list) : t list bot =
    applyf filter_cos_itv i r
(*
  (* r = tan i => i = arctan r *)
  let filter_tan i r =
    let atan_r = atan r in
    let (aux, _) = div (add i (of_bound pi_half)) (of_bound pi) in
    Format.printf "atan = %s\n aux = %s\n" (to_string atan_r) (Bot.bot_to_string to_string aux);
    match aux with
    | Bot -> Bot
    | Nb (p1,p2) -> 
      let idx = ref ((int_of_float (B.to_float_up (B.floor p1))) - 1) in
      let itv = ref (meet i (add atan_r (mul (of_bound pi) (of_int !idx)))) in
      while !idx < (int_of_float (B.to_float_down p2)) && is_Bot !itv do
        idx := !idx + 1;
        itv := meet i (add atan_r (mul (of_bound pi) (of_int !idx)));
      done;
      if (is_Bot !itv) then
        Bot
      else
        let idx = ref ((int_of_float (B.to_float_up (B.floor p2))) + 1) in
        let itv' = ref (meet i (add atan_r (mul (of_bound pi) (of_int !idx)))) in
        while !idx > (int_of_float (B.to_float_down p1)) && is_Bot !itv' do
          idx := !idx - 1;
          itv' := meet i (add atan_r (mul (of_bound pi) (of_int !idx)));
        done;
        Nb (Bot.join_bot2 join !itv !itv')

  (* r = asin i => i = sin r *)
  let filter_asin i r =
    meet i (sin r)

  (* r = acos i => i = cos r *)
  let filter_asin i r =
    meet i (cos r)

  (* r = atan i => i = tan r *)
  let filter_atan i r =
    meet i (tan r)

  (* r = exp i => i = log r *)
  let filter_exp i r =
    meet_bot2 meet i (log r)

  (* r = log i => i = exp r *)
  let filter_log i r =
    meet i (exp r)

  (* r = i ** n => i = nroot i *)
  let filter_pow (i:t) n (r:t) =
    (* let nri = n_root r n in *)
    (* let fnri = meet_bot meet i (n_root r n) in *)
    (* Format.printf "%s %s %s => %s => %s%!\n" (to_string i) (to_string r) (to_string n) (Bot.bot_to_string to_string nri) (Bot.bot_to_string to_string fnri); *)
    merge_bot2 (meet_bot meet i (n_root r n)) (Nb n)

  (* r = nroot i => i = r ** n *)
  let filter_root i r n =
    meet i (pow r n)
 *)

end)
    
module U_ItvF = Union_Itv(Itv.ItvF)
(* module ItvQ = Itv(Bound_rational) *)
