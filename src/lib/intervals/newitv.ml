open Bot
open Bound_sig

module Make(B:BOUND) = struct

  module B = B

  type bound = B.t

  type kind = Strict | Large

  let sym = function
    | Large,x -> Strict,x
    | Strict,x -> Large,x

  let stricten = function
    | Large,x -> Strict,x
    | x -> x

  type real_bound = kind * bound

  let mix k1 k2 =
    match k1,k2 with
    | Large,Large -> Large
    | _ -> Strict

  let bound_arith ((k1,b1):real_bound) ((k2,b2):real_bound) f =
    (mix k1 k2),(f b1 b2)

  let bound_mul_up   = B.bound_mul B.mul_up
  let bound_mul_down = B.bound_mul B.mul_down
  let bound_div_up   = B.bound_div B.div_up
  let bound_div_down = B.bound_div B.div_down

  let ( +@ ) rb1 rb2 = bound_arith rb1 rb2 B.add_up

  let ( +$ ) rb1 rb2 = bound_arith rb1 rb2 B.add_down

  let ( -@ ) rb1 rb2 = bound_arith rb1 rb2 B.sub_up

  let ( -$ ) rb1 rb2 = bound_arith rb1 rb2 B.sub_down

  let ( *@ ) rb1 rb2 = bound_arith rb1 rb2 bound_mul_up

  let ( *$ ) rb1 rb2 = bound_arith rb1 rb2 bound_mul_down

  let ( /@ ) ((k1, b1) as rb1) ((k2, b2) as rb2) = bound_arith rb1 rb2 bound_div_up

  (*let ( /$ ) rb1 rb2 = *)
  let ( /$ ) ((k1, b1) as rb1) ((k2, b2) as rb2) = bound_arith rb1 rb2 bound_div_down

  type t = real_bound * real_bound

  (* returns the half space defined by a bound and a direction.
     - true for going toward +oo
     - false for going toward -oo
     Ex: half_space (Strict,0) true gives ]0; +oo[ *)
  let half_space (k,b) dir =
    (match (k,dir) with
    | Strict, false  -> B.gt
    | Strict, true -> B.lt
    | Large , false  -> B.geq
    | Large , true -> B.leq) b

  (* check if a value is in a half space *)
  let in_half (k,b) dir v : bool = v |> half_space (k,b) dir

  let cmp_low ((_,b1) as l1) ((_,b2) as l2) =
    if in_half l1 true b2 then -1
    else if in_half l2 true b1 then 1
    else 0

  (* compare two up bounds *)
  let cmp_up ((_,b1) as u1) ((_,b2) as u2) =
    if in_half u1 false b2 then 1
    else if in_half u2 false b1 then -1
    else 0

  let gt_low b1 b2 = cmp_low b1 b2 = 1

  let lt_low b1 b2 = cmp_low b1 b2 = -1

  let gt_up b1 b2 = cmp_up b1 b2 = 1

  let lt_up b1 b2 = cmp_up b1 b2 = -1

  (* returns the lower bound two low bounds *)
  let min_low l1 l2 = if cmp_low l1 l2 = 1 then l2 else l1

  (* returns the higher bound two low bounds *)
  let max_low l1 l2 = if cmp_low l1 l2 = 1 then l1 else l2

  (* returns the lower bound two high bounds *)
  let min_up u1 u2 = if cmp_up u1 u2 = 1 then u2 else u1

  (* returns the higher bound two high bounds *)
  let max_up u1 u2 = if cmp_up u1 u2 = 1 then u1 else u2

  (* maps empty intervals to explicit bottom *)
  let check_bot ((((_,l) as b1),((_,h)as b2)) as itv) : t bot =
    if in_half b1 true h && in_half b2 false l then Nb itv else Bot

  (* not all pairs of rationals are valid intervals *)
  let validate x =
    if check_bot x = Bot then failwith "invalid interval" else x

    (************************************************************************)
  (* CONSTRUCTORS AND CONSTANTS *)
  (************************************************************************)

  let large (x:B.t) (y:B.t) : t = validate ((Large,x),(Large,y))

  let strict (x:B.t) (y:B.t) : t = validate ((Strict,x),(Strict,y))

  let large_strict (x:B.t) (y:B.t) : t = validate ((Large,x),(Strict,y))

  let strict_large (x:B.t) (y:B.t) : t = validate ((Strict,x),(Large,y))

  let of_bound (x:B.t) : t = large x x

  let zero : t = of_bound B.zero

  let one : t = of_bound B.one

  let minus_one : t = of_bound B.minus_one

  let top_real : t = (Strict,B.minus_inf), (Strict,B.inf)
  let top_int : t = (Strict,B.minus_inf), (Strict,B.inf)
  let top = top_real

  let zero_one : t = large B.zero B.one

  let minus_one_zero : t = large B.minus_one B.zero

  let minus_one_one : t = large B.minus_one B.one

  let positive : t = large_strict B.zero B.inf

  let negative : t = strict_large B.minus_inf B.zero

  let of_bounds = large

  let of_ints (l:int) (h:int) : t =
    of_bounds (B.of_int_down l) (B.of_int_up h)

  let of_int (x:int) : t = of_ints x x

  let of_floats (l:float) (h:float) : t =
    of_bounds (B.of_float_down l) (B.of_float_up h)

  let of_float (x:float) : t = of_floats x x

  let of_rats (l:Mpqf.t) (h:Mpqf.t) : t = of_bounds (B.of_rat_down l) (B.of_rat_up h)

  let of_rat (x:Mpqf.t) = of_rats x x

  let hull (x:B.t) (y:B.t) : t =
    try large x y
    with Failure _ -> large y x

  let half_sup (x:B.t) : t = large_strict x B.inf

  let half_inf (x:B.t) : t = strict_large B.minus_inf x


  (************************************************************************)
  (* PRINTING *)
  (************************************************************************)
  let to_string (((kl,l),(kh,h)):t) : string =
    Printf.sprintf
      "%c%a;%a%c"
      (if kl = Strict then ']' else '[')
      B.sprint l
      B.sprint h
      (if kh = Large then ']' else '[')

  let print fmt (x:t) = Format.fprintf fmt "%s" (to_string x)

  let to_expr (((kl, l), (kh, h)):t) =
    match kl, kh with
      | Strict, Strict -> ((Csp.GT, Csp.Cst(B.to_rat l)),
                           (Csp.LT, Csp.Cst(B.to_rat h)))
      | Strict, Large -> ((Csp.GT, Csp.Cst(B.to_rat l)),
                          (Csp.LEQ, Csp.Cst(B.to_rat h)))
      | Large, Strict -> ((Csp.GEQ, Csp.Cst(B.to_rat l)),
                          (Csp.LT, Csp.Cst(B.to_rat h)))
      | Large, Large -> ((Csp.GEQ, Csp.Cst(B.to_rat l)),
                         (Csp.LEQ, Csp.Cst(B.to_rat h)))

   (************************************************************************)
  (* SET-THEORETIC *)
  (************************************************************************)

  (* operations *)
  (* ---------- *)
  let join ((l1,h1):t) ((l2,h2):t) : t =
    min_low l1 l2, max_up h1 h2

  let meet ((l1,h1):t) ((l2,h2):t) : t bot =
    check_bot (max_low l1 l2, min_up h1 h2)

  (* ---------- *)
  (* predicates *)
  (* ---------- *)
  let equal ((l1,h1):t) ((l2,h2):t) : bool =
    let equal_bound (k1,b1) (k2,b2) =
      k1 = k2 && B.equal b1 b2
    in equal_bound l1 l2 && equal_bound h1 h2

  (* i1 in i2*)
  let subseteq i1 i2 : bool =
    join i1 i2 |> equal i2

  let contains ((l,h):t) (x:B.t) : bool =
    in_half l true x && in_half h false x

  let contains_float (i:t) (x:float) : bool =
    contains i (B.of_float_up x)
    (*TODO: improve rounding errors resiliance *)

  let intersect i1 i2 : bool =
    meet i1 i2 <> Bot

  let is_finite ((_,x) : real_bound) : bool =
    B.classify x = B.FINITE

  let is_bounded ((l,h):t) =
    is_finite l && is_finite h

  let is_singleton ((l,h):t) : bool =
    is_finite l && B.equal (snd l) (snd h)

  let range (((_,l),(_,h)): t) = B.sub_up h l


  let float_size (i:t) : float =
    B.to_float_up (range i)

  (* split *)
  (* ----- *)

  (* split priority *)
  let score itv = float_size itv


  (* length of the intersection (>= 0) *)
  let overlap i1 i2 =
    match meet i1 i2 with
    | Bot -> B.zero
    | Nb i -> range i

  let magnitude (((_,l),(_,h)): t) : B.t =
    B.max (B.abs l) (B.abs h)

  let mean (((_,l) as low, ((_,h) as high)):t) : B.t =
    match is_finite low, is_finite high with
    | true,true -> B.div_up (B.add_up l h) B.two
    | true,false ->
       if B.sign l < 0 then B.zero
       else if B.sign l = 0 then B.one
       else B.mul_up l B.two
    | false,true ->
       if B.sign h > 0 then B.zero
       else if B.sign h = 0 then B.minus_one
       else B.mul_down h B.two
    | false,false -> B.zero

  let split_on_value ((l,h) :t) (x : B.t) : t list =
    let rec aux acc cur bounds =
      match bounds with
      |  hd::tl ->
	       let itv = validate (cur,(Large,hd)) in
           aux (itv::acc) (Strict,hd) tl
      | [] ->
           let itv = validate (cur,h) in
           itv::acc
    in aux [] l [x]

  (* splits in two, around the middle *)
  let split (i:t) : t list = split_on_value i (mean i)

  let prune ((l,h):t) ((l',h'):t) : t list  =
    match (gt_low l' l), (lt_up h' h) with
    | true , true  -> [(l, (sym l')); ((sym h'), h)]
    | true , false -> [(l, (sym l'))]
    | false, true  -> [((sym h'), h)]
    | false, false -> []

  let prune = Some prune

  (************************************************************************)
  (* INTERVAL ARITHMETICS (FORWARD EVALUATION) *)
  (************************************************************************)

  (* few utilities based on monotony *)
  (***********************************)

  (* f [a;b] -> [f(a);f(b)] when f is monotonic increasing *)
  let mon_incr (f_down,f_up) ((kl,low),(kh,high)) =
    (kl,(f_down low)),(kh,(f_up high))

  (* f [a;b] -> [f(b);f(a)] when f is monotonic decreasing *)
  let mon_decr (f_down,f_up) ((kl,low),(kh,high)) =
    (kh,(f_down high)),(kl,(f_up low))

  (* when f changes its monotony in "c";
     - if f ↗↘ then f [a;b] = [f(a);f(c)] U [f(b);f(c)]
     - if f ↘↗ then f [a;b] = [f(c);f(a)] U [f(c);f(b)] *)
  let mon_2 f c ((a,b) as itv) first =
    let f1,f2 = if first then mon_incr,mon_decr else mon_decr,mon_incr in
    if subseteq itv (half_inf c) then f1 f itv
    else if subseteq itv (half_sup c) then f2 f itv
    else join (f1 f (a,(Large,c))) (f2 f ((Large,c),b))

  (* arithmetical functions *)
  (**************************)

  let neg (i:t) : t = mon_decr (B.neg,B.neg) i

  let abs (i:t) : t =
    let f = B.abs,B.abs in
    if subseteq i positive then mon_incr f i
    else if subseteq i negative then mon_decr f i
    else mon_2 (B.abs,B.abs) B.zero i false

  let add ((l1,h1):t) (l2,h2:t) : t = l1 +$ l2, h1 +@ h2

  let sub ((l1,h1):t) (l2,h2:t) : t = l1 -$ h2, h1 -@ l2

  let mul ((l1,h1):t) ((l2,h2):t) : t =
    min_low (min_low (l1 *$ l2) (l1 *$ h2)) (min_low (h1 *$ l2) (h1 *$ h2)),
    max_up  (max_up  (l1 *@ l2) (l1 *@ h2)) (max_up  (h1 *@ l2) (h1 *@ h2))

  let div_sgn ((l1,h1):t) ((l2,h2):t) : t =
    if  B.sign (snd h2) = 0 then
      (Large, B.minus_inf),
      max_up  (l1 /@ l2) (h1 /@ l2)
    else if B.sign (snd l2) = 0 then
      (min_low  (l1 /$ h2) (h1 /$ h2)),
      (Large, B.inf)
    else
      min_low (min_low (l1 /$ l2) (l1 /$ h2)) (min_low (h1 /$ l2) (h1 /$ h2)),
      max_up  (max_up  (l1 /@ l2) (l1 /@ h2)) (max_up  (h1 /@ l2) (h1 /@ h2))

  let div (i1:t) (i2:t) : t bot =
    let pos = (lift_bot (div_sgn i1)) (meet i2 positive) in
    let neg = (lift_bot (div_sgn i1)) (meet i2 negative) in
    join_bot2 join pos neg


  let sqrt (itv:t) : t bot =
    match meet itv positive with
    | Bot -> Bot
    | Nb itv -> Nb (mon_incr (B.sqrt_down,B.sqrt_up) itv)

  let pow (itv:t) ((l,h):t) =
    let is_int (l,x) = l=Large && B.floor x=x in
    if l=h && is_int l then
      let i = B.to_float_down (snd l) |> int_of_float in
      let f_down_up = ((fun b -> B.pow_down b i),(fun b -> B.pow_up b i)) in
      let pow_odd x : t = mon_incr f_down_up x
      and pow_even x : t = mon_2 f_down_up B.zero x false in
      match i with
      | 0 -> one
      | 1 -> itv
      | x when x > 1 && i mod 2 = 1 -> pow_odd itv
      | x when x > 1 -> pow_even itv
      | _ -> failwith "cant handle negatives powers"
    else failwith  "cant handle non_singleton powers"

  (* nth-root *)
  let n_root (itv:t) ((l,h):t) : t bot =
    let is_int (l,x) = l=Large && B.floor x = x in
    if l=h && is_int l then
      let i = B.to_float_down (snd l) |> int_of_float in
      let f_down_up = ((fun b -> B.root_down b i),(fun b -> B.root_up b i)) in
      let root_odd x : t = mon_incr (f_down_up) x
      and root_even x : t bot =
        match meet x positive with
        | Bot -> Bot
        | Nb x ->
           let pos_part = mon_incr (f_down_up) x in
           Nb (join pos_part (neg pos_part))
      in
      match i with
      | 1 -> Nb itv
      | x when x > 1 && i mod 2 = 1 -> Nb(root_odd itv)
      | x when x > 1 -> root_even itv
      | _ -> failwith "can only handle stricly positive roots"
    else failwith  "cant handle non_singleton roots"



  (*the two closest floating boundaries of pi*)
  let pi_up = B.of_float_up 3.14159265358979356
  let pi_down = B.of_float_down 3.14159265358979312

  (* it improves soundness to use those *)
  let i_pi:t= (Large, pi_down), (Large, pi_up)
  let i_pi_half = div i_pi (of_int 2) |> debot
  let i_two_pi = add i_pi i_pi
  let i_three_half_of_pi = div (add i_two_pi i_pi) (of_int 2) |> debot

  let b_one = (Large, B.one)
  let b_minus_one = (Large, B.minus_one)


  type quadrant = | One
		  | Two
		  | Three
		  | Four

  (* Returns the quadrant in which the bound is. the value must be in [0, 2pi[ *)
  let quadrant value =
    if B.leq value (snd (fst i_pi_half)) then One
    else if B.leq value pi_down then Two
    else if B.leq value (snd (fst i_three_half_of_pi)) then Three
    else Four

  (* A bound is scaled to the range [0, 2pi[ *)
  let scale_to_two_pi value =
    let q = B.floor (B.div_up value (snd (fst i_two_pi))) in
    B.sub_up value (B.mul_up (snd (fst i_two_pi)) q)

  (* The interval is scaled to the range [0, 2pi[ *)
  let scale_to_two_pi_itv (l, h) =
    if B.geq l B.zero && B.lt h (snd (fst i_two_pi)) then (l,h)
    else (scale_to_two_pi l, scale_to_two_pi h)


  let bfg f g (k1, v1) (k2, v2) =
    let v1' = g v1
    and v2' = g v2 in
    if B.equal (f v1' v2') v1' then (k1, v1')
    else (k2, v2')

  let bf f (k1, v1) (k2, v2) =
    if B.equal (f v1 v2) v1 then (k1, v1)
    else (k2, v2)


  let uf f (k, v) =
    (k, f v)


  let ln (i:t) : t bot =
    match meet i positive with
    | Bot -> Bot
    | Nb itv -> Nb (mon_incr (B.ln_down,B.ln_up) i)

  let exp (i:t) = mon_incr (B.exp_down,B.exp_up) i

  (*the two closest floating boundaries of pi*)
  let ln10_up = B.ln_up (B.of_int_up 10)
  let ln10_down = B.ln_down (B.of_int_down 10)

  (* it improves soundness to use those *)
  let i_ln10:t= (Large, ln10_down), (Large, ln10_up)

  (* interval log *)
  let log itv =
    let itv' = ln itv in
    match itv' with
    | Bot -> Bot
    | Nb i -> div i i_ln10

  (* interval min *)
  let min (((kl1, l1), (kh1, h1)):t) (((kl2, l2), (kh2, h2)):t) : t =
    let low =
      if l1 < l2 then kl1,l1
      else if l2 < l1 then kl2,l2
      else (* l1 = l2 *) (if kl1 = Large then kl1 else kl2),l1
    and high =
      if h1 < h2 then kh1,h1
      else if h2 < h1 then kh2,h2
      else (* h1 = h2 *) (if kh1 = Strict then kh1 else kh2),h2
    in
    (low,high)

  (* interval max *)
  let max (((kl1, l1), (kh1, h1)):t) (((kl2, l2), (kh2, h2)):t) : t =
    let low =
      if l1 > l2 then kl1,l1
      else if l2 > l1 then kl2,l2
      else (* l1 = l2 *) (if kl1 = Strict then kl1 else kl2),l1
    and high =
      if h1 > h2 then kh1,h1
      else if h2 > h1 then kh2,h2
      else (* h1 = h2 *) (if kh1 = Large then kh1 else kh2),h2
    in
    (low,high)

  (** runtime functions **)
  let eval_fun name args : t bot =
    let arity_1 (f: t -> t) : t bot =
       match args with
       | [i] -> Nb (f i)
      | _ -> failwith (Format.sprintf "%s expect one argument" name)
    in
    let arity_1_bot (f: t -> t bot) : t bot =
      match args with
      | [i] ->
         (match f i with
          | Bot -> Bot
          | Nb i -> Nb i)
      | _ -> failwith (Format.sprintf "%s expect one argument" name)
    in
    let arity_2 (f: t -> t -> t) : t bot  =
      match args with
      | [i1;i2] -> Nb (f i1 i2)
      | _ -> failwith (Format.sprintf "%s expect two arguments" name)
    in
    let arity_2_bot (f: t -> t -> t bot) : t bot  =
      match args with
      | [i1;i2] ->
         (match f i1 i2 with
          | Bot -> Bot
          | Nb(i) -> Nb i)
      | _ -> failwith (Format.sprintf "%s expect two arguments" name)
    in
    match name with
    | "pow"   -> arity_2 pow
    | "nroot" -> arity_2_bot n_root
    | "sqrt"  -> arity_1_bot sqrt
    | "exp"   -> arity_1 exp
    | "ln"    -> arity_1_bot ln
    | "log"   -> arity_1_bot log
    (* min max *)
    | "max"   -> arity_2 max
    | "min"   -> arity_2 min
    | s -> failwith (Format.sprintf "unknown eval function : %s" s)

  (************************************************************************)
  (* FILTERING (TEST TRANSFER FUNCTIONS) *)
  (************************************************************************)

  let merge_check a b c d =
    merge_bot2 (check_bot (a,b)) (check_bot (c,d))

  let filter_leq ((l1,h1):t) ((l2,h2):t) : (t * t) bot =
    merge_check l1 (min_up h1 h2) (max_low l1 l2) h2

  let filter_lt ((l1,h1):t) ((l2,h2):t) : (t*t) bot =
    merge_check l1 (min_up h1 (stricten h2)) (max_low (stricten l1) l2) h2

  let filter_eq (i1:t) (i2:t) : (t*t) bot =
    lift_bot (fun x -> x,x) (meet i1 i2)

  let filter_neq ((l1,_) as i1:t) ((l2,_) as i2:t) : (t*t) bot =
    if is_singleton i1 && is_singleton i2 && equal i1 i2 then Bot
    else Nb (i1,i2)

  (* arithmetic *)
  (* --------- *)

  (* r = -i => i = -r *)
  let filter_neg (i:t) (r:t) : t bot = meet i (neg r)

  (* r = |i| => i = r U -r *)
  let filter_abs (i:t) (r:t) : t bot =
    assert (subseteq r positive);
    meet i (join r (neg r))

  (* r = i + c => i = r - c *)
  let filter_add_f (i:t) (c:t) (r:t) : t bot =
    meet i (sub r c)

  (* r = i - c => i = r + c *)
  let filter_sub_f (i:t) (c:t) (r:t) : t bot =
    meet i (add c r)

  (* r = i*c => (i = r/c \/ c=r=0) *)
  let filter_mul_f (i:t) (c:t) (r:t) : t bot =
    if contains r B.zero && contains c B.zero then Nb i
    else strict_bot (meet i) (div r c)

  (* r = i/c => i = r*c *)
  let filter_div_f (i:t) (c:t) (r:t) : t bot =
    meet i (mul c r)

  (* r = i ** n => i = nroot r *)
  let filter_pow_f (i:t) n (r:t) =
    meet_bot meet i (n_root r n)

  (* r = nroot i => i = r ** n *)
  let filter_root_f i r n =
    meet i (pow r n)


  (* r = i1+i2 => i1 = r-i2 /\ i2 = r-i1 *)
  let filter_add (i1:t) (i2:t) (r:t) : (t*t) bot =
    merge_bot2 (meet i1 (sub r i2)) (meet i2 (sub r i1))

  (* r = i1-i2 => i1 = i2+r /\ i2 = i1-r *)
  let filter_sub (i1:t) (i2:t) (r:t) : (t*t) bot =
    merge_bot2 (meet i1 (add i2 r)) (meet i2 (sub i1 r))

  (* r = i*c => (i = r/c \/ c=r=0) *)
  let filter_mul_cst (i:t) (c:t) (r:t) : (t*t) bot =
    merge_bot2
      (if contains r B.zero && contains c B.zero then Nb i
      else strict_bot (meet i) (div r c))
      (Nb c)

  (* r = i*c => (i = r/c \/ c=r=0) *)
  let filter_cst_mul (i:t) (c:t) (r:t) : (t*t) bot =
    merge_bot2
      (Nb c)
      (if contains r B.zero && contains c B.zero then Nb i
      else strict_bot (meet i) (div r c))

  (* r = i1*i2 => (i1 = r/i2 \/ i2=r=0) /\ (i2 = r/i1 \/ i1=r=0) *)
  let filter_mul (i1:t) (i2:t) (r:t) : (t*t) bot =
    merge_bot2
      (if contains r B.zero && contains i2 B.zero then Nb i1
      else strict_bot (meet i1) (div r i2))
      (if contains r B.zero && contains i1 B.zero then Nb i2
       else strict_bot (meet i2) (div r i1))

   (* r = i1/i2 => i1 = i2*r /\ (i2 = i1/r \/ i1=r=0) *)
  let filter_div (i1:t) (i2:t) (r:t) : (t*t) bot =
    merge_bot2
      (meet i1 (mul i2 r))
      (if contains r B.zero && contains i1 B.zero then Nb i2
      else strict_bot (meet i2) (div i1 r))

  (* r = sqrt i => i = r*r or i < 0 *)
  let filter_sqrt (((k_il,il),ih) as i:t) ((rl,rh):t) : t bot =
    let rr = rl *$ rl, rh *@ rh in
    if B.sign il > 0 || (B.sign il = 0 && k_il = Large) then meet i rr
    else meet i ((Strict,B.minus_inf), snd rr)

  let compute_itv itv itv' i i' =
    let aux =
      if i mod 2 = 0 then add itv' (mul i_pi (of_int i))
      else sub (mul i_pi (of_int i')) itv'
    in meet itv aux

  (* r = exp i => i = ln r *)
  let filter_exp i r = meet_bot meet i (ln r)

  (* r = ln i => i = exp r *)
  let filter_ln i r = meet i (exp r)

  (* r = log i => i = *)
  let filter_log i r = failwith "todo filter_log"

  (* r = i ** n => i = nroot r *)
  let filter_pow (i:t) n (r:t) =
    merge_bot2 (meet_bot meet i (n_root r n)) (Nb n)

  (* r = nroot i => i = r ** n *)
  let filter_root i r n =
    merge_bot2 (meet i (pow r n)) (Nb n)

  (* r = min (i1, i2) *)
  let filter_min (l1, u1) (l2, u2) (lr, ur) =
    merge_bot2 (check_bot (bf B.max l1 lr, bf B.max u1 ur)) (check_bot (bf B.max l2 lr, bf B.max u2 ur))

  (* r = max (i1, i2) *)
  let filter_max (l1, u1) (l2, u2) (lr, ur) =
    merge_bot2 (check_bot (bf B.min l1 lr, bf B.min u1 ur)) (check_bot (bf B.min l2 lr, bf B.min u2 ur))

  let filter_fun name args r : (t list) bot =
    let arity_1 (f: t -> t -> t bot) : (t list) bot =
      match args with
      | [i] ->
         (match f i r with
         | Bot -> Bot
         | Nb i -> Nb [i])
      | _ -> failwith (Format.sprintf "%s expect one argument" name)
    in
    let arity_2 (f: t -> t -> t -> (t*t) bot) : (t list) bot  =
      match args with
      | [i1;i2] ->
         (match f i1 i2 r with
          | Bot -> Bot
          | Nb(i1,i2) -> Nb[i1;i2])
      | _ -> failwith (Format.sprintf "%s expect two arguments" name)
    in
    match name with
    | "sqrt" -> arity_1 filter_sqrt
    | "exp"  -> arity_1 filter_exp
    | "ln"   -> arity_1 filter_ln
    | "max"  -> arity_2 filter_max
    | "min"  -> arity_2 filter_min
    | s -> failwith (Format.sprintf "unknown filter function : %s" s)

  let to_float_range ((_,l),(_,h)) = (B.to_float_down l),(B.to_float_up h)

  let to_rational_range ((_,l),(_,h)) = (B.to_rat l),(B.to_rat h)

  (* returns the type annotation of the represented values *)
  let to_annot _ = Csp.Real

  (* generate a random float between l and h *)
  let spawn (((_,l),(_,h)):t) =
    (* we convert toward the inside of the itv to avoid false negative *)
    let r = Random.float 1. in
    let res = B.add_up l (B.mul_up (B.sub_up h l) (B.of_float_up r)) in
    B.to_float_up res

end

module Test = Make(Bound_float)
module TestQ = Make(Bound_mpqf)
