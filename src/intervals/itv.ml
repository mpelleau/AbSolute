(*
   Generic intervals.
   Can be instantiated with any bound type.
   An interval is a pair `(l,u)` where `l` is the lower bound and `u` is the upper bound.
   Consequently it does not handle "holes" in the domain.
*)


open Bot
open Bound_sig


module Itv(B:BOUND) = struct

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
    | B.INVALID,_ | _,B.INVALID  | B.MINF,_ | _,B.INF
    | _ when B.gt l h -> invalid_arg "int.validate"
    | _ -> l,h

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

  let top_real : t = B.minus_inf, B.inf
  let top_int : t = B.minus_inf, B.inf
  let top : t = B.minus_inf, B.inf

  let zero_one : t = B.zero, B.one

  let minus_one_zero : t = B.minus_one, B.zero

  let minus_one_one : t = B.minus_one, B.one

  let positive : t = B.zero, B.inf

  let negative : t = B.minus_inf, B.zero

  let of_bounds (l:bound) (h:bound) = validate (l,h)

  let of_ints (l:int) (h:int) : t = of_bounds (B.of_int_down l) (B.of_int_up h)

  let of_int (x:int) = of_ints x x

  let of_rats (l:Bound_rat.t) (h:Bound_rat.t) : t = of_bounds (B.of_rat_down l) (B.of_rat_up h)

  let of_rat (x:Bound_rat.t) = of_rats x x

  let of_floats (l:float) (h:float) : t = of_bounds (B.of_float_down l) (B.of_float_up h)

  let of_float (x:float) = of_floats x x

  let hull (x:B.t) (y:B.t) = B.min x y, B.max x y

  (************************************************************************)
  (* PRINTING *)
  (************************************************************************)

  let pp_print_bound fmt (b:B.t) =
    if B.ceil b = b then
      Format.fprintf fmt "%0F" (B.to_float_down b)
    else
      Format.fprintf fmt "%a" Format.pp_print_float (B.to_float_down b)

  (* printer *)
  let print fmt ((l,h):t) =
    if l = h
    then pp_print_bound fmt l
    else Format.fprintf fmt "[%a;%a]" pp_print_bound l pp_print_bound h

  let to_expr ((l, h):t) =
    ((Csp.GEQ, Csp.Cst(B.to_rat l, Csp.Real)),
     (Csp.LEQ, Csp.Cst(B.to_rat h, Csp.Real)))

  (************************************************************************)
  (* SET-THEORETIC *)
  (************************************************************************)


  (* operations *)
  (* ---------- *)

  let join ((l1,h1):t) ((l2,h2):t) : t =
    B.min l1 l2, B.max h1 h2

  let meet ((l1,h1):t) ((l2,h2):t) : t bot =
    check_bot (B.max l1 l2, B.min h1 h2)

  let negative_part ((l1,h1):t) : t bot =
    if B.geq l1 B.zero then Bot else
      check_bot (l1,(B.min h1 B.zero))

  let positive_part ((l1,h1):t) : t bot =
    if B.leq h1 B.zero then Bot else
      check_bot ((B.max l1 B.zero),h1)

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

  let contains_float ((l,h):t) (x:float) : bool =
    B.leq l (B.of_float_down x) && B.leq (B.of_float_up x) h

  (* mesure *)
  (* ------ *)

  (* length of the intersection (>= 0) *)
  let overlap ((l1,h1):t) ((l2,h2):t) : B.t  =
    B.max B.zero (B.sub_up (B.min h1 h2) (B.max l1 l2))

  let range ((l,h):t) : B.t = (B.sub_up h l)

  let float_size (itv:t) : float =
    B.to_float_up (range itv)

  (* split *)
  (* ----- *)

  (* split priority *)
  let score itv = float_size itv

  (* find the mean of the interval;
     when a bound is infinite, then "mean" is some value strictly inside the
     interval
   *)
  let mean ((l,h):t) : B.t =
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

  let split_on_value ((l,h) :t) (x : B.t) : t list =
    let rec aux acc cur bounds =
      match bounds with
      |  hd::tl ->
	       let itv = validate (cur,hd) in
	       aux (itv::acc) hd tl
      | [] ->
	       let itv = validate (cur,h) in
	       itv::acc
    in aux [] l [x]

  let split_on (i : t) (x: Bound_rat.t) : t list =
    split_on_value i (B.of_rat_up x)

  (* splits in two, around the middle *)
  let split (i:t) : t list = split_on_value i (mean i)

  (* there's no exact difference operator on itv with closed bounds *)
  let prune = None

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

  let bound_mul_up   = B.bound_mul B.mul_up
  let bound_mul_down = B.bound_mul B.mul_down

  let mix4 up down ((l1,h1):t) ((l2,h2):t) =
    B.min (B.min (down l1 l2) (down l1 h2)) (B.min (down h1 l2) (down h1 h2)),
    B.max (B.max (up   l1 l2) (up   l1 h2)) (B.max (up   h1 l2) (up   h1 h2))

  let mul =
    mix4 bound_mul_up bound_mul_down

  let bound_div_up   = B.bound_div B.div_up
  let bound_div_down = B.bound_div B.div_down

  (* helper: assumes i2 has constant sign *)
  let div_sign ((l1,h1) as i1) ((l2,h2) as i2) =
    if  B.sign h2 = 0 then
      B.minus_inf, B.max (bound_div_up l1 l2) (bound_div_up h1 l2)
    else if B.sign l2 = 0 then
      B.min (bound_div_down l1 h2) (bound_div_down h1 h2), B.inf
    else
    mix4 bound_div_up bound_div_down i1 i2

  (* return valid values (possibly Bot) + possible division by zero *)
  let div (i1:t) (i2:t) : t bot =
    (* split into positive and negative dividends *)
    let pos = (lift_bot (div_sign i1)) (positive_part i2)
    and neg = (lift_bot (div_sign i1)) (negative_part i2) in
    (* joins the result*)
    join_bot2 join pos neg

  (* interval square root *)
  let sqrt ((l,h):t) : t bot =
    if B.sign h < 0 then Bot else
    let l = B.max l B.zero in
   Nb (B.sqrt_down l, B.sqrt_up h)

  (* useful operators on intervals *)
  let ( +@ ) = add
  let ( -@ ) = sub
  let ( *@ ) = mul
  let ( /@ ) = div

  let ln10 = B.of_float_up 2.3025850

  (* interval exp *)
  let exp (l,h) = (B.exp_down l,B.exp_up h)

  (* interval ln *)
  let ln (l,h) =
    if B.leq h B.zero then Bot
    else if B.leq l B.zero then Nb (B.minus_inf,B.ln_up h)
    else Nb (B.ln_down l,B.ln_up h)

  (* interval log *)
  let log itv =
    let itv' = ln itv in
    match itv' with
    | Bot -> Bot
    | Nb (l', h') -> div (l',h') (of_bound ln10)

  (* powers *)
  let pow ((il,ih):t) ((l,h):t) =
    if l=h && B.floor l = l then
      let p = B.to_float_down l |> int_of_float in
      match p with
      | 0 -> one
      | 1 -> (il, ih)
      | x when x > 1 && p mod 2 = 1 -> (B.pow_down il p, B.pow_up ih p)
      | x when x > 1 && B.even l ->
        if B.leq il B.zero && B.geq ih B.zero then
	  (B.zero, B.max (B.pow_up il p) (B.pow_up ih p))
        else if B.geq il B.zero then
	  (B.pow_down il p, B.pow_up ih p)
        else (B.pow_down ih p, B.pow_up il p)
      | _ -> failwith "cant handle negatives powers"
    else failwith  "cant handle non_singleton powers"

  (* nth-root *)
  let n_root ((il,ih):t) ((l,h):t) =
    if B.equal l h && B.floor l = l then
      let p = B.to_float_down l |> int_of_float in
      match p with
      | 1 -> Nb (il, ih)
      | x when x > 1 && B.odd l ->
	Nb (B.root_down il p, B.root_up ih p)
      | x when x > 1 && B.even l ->
        if B.lt ih B.zero then Bot
        else if B.leq il B.zero then Nb (B.neg (B.root_up ih p), B.root_up ih p)
        else
          Nb (B.min (B.neg (B.root_down il p)) (B.neg (B.root_down ih p)), B.max (B.root_up il p) (B.root_up ih p))
      | _ -> failwith "can only handle stricly positive roots"
    else failwith  "cant handle non_singleton roots"


  (* interval min *)
  let min ((l1, u1):t) ((l2, u2):t) =
    validate (B.min l1 l2, B.min u1 u2)

  (* interval max *)
  let max ((l1, u1):t) ((l2, u2):t) =
    validate (B.max l1 l2, B.max u1 u2)

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


  (* tests *)
  (* ----- *)

  let filter_leq ((l1,h1):t) ((l2,h2):t) : (t*t) bot =
    merge_bot2 (check_bot (l1, B.min h1 h2)) (check_bot (B.max l1 l2, h2))

  let filter_lt ((l1,_) as i1:t) ((l2,h2) as i2:t) : (t*t) bot =
    if is_singleton i1 && is_singleton i2 && B.equal l1 l2 then Bot
    else if B.leq h2 l1 then Bot
    else filter_leq i1 i2

  let filter_eq (i1:t) (i2:t) : (t*t) bot =
    (*Format.printf "%a = %a\n" print i1 print i2;*)
    lift_bot (fun x -> x,x) (meet i1 i2)

  let filter_neq ((l1,_) as i1:t) ((l2,_) as i2:t) : (t*t) bot =
    if is_singleton i1 && is_singleton i2 && B.equal l1 l2 then Bot
    else Nb (i1,i2)

  (* arithmetic *)
  (* --------- *)

  (* r = -i => i = -r *)
  let filter_neg (i:t) (r:t) : t bot =
    meet i (neg r)

  let filter_abs ((il,ih) as i:t) ((rl,rh) as r:t) : t bot =
    if B.sign il >= 0 then meet i r
    else if B.sign ih <= 0 then meet i (neg r)
    else meet i (B.neg rh, rh)

  (* r = i + c => i = r - c *)
  let filter_add_f (i:t) (c:t) (r:t) : t bot =
    meet i (sub r c)

  (* r = i - c => i = r + c *)
  let filter_sub_f (i:t) (c:t) (r:t) : t bot =
    meet i (add c r)

  (* r = i*c => (i = r/c \/ c=r=0) *)
  let filter_mul_f (i:t) (c:t) (r:t) : t bot =
    if contains r B.zero && contains c B.zero then Nb i
    else Bot.strict_bot (meet i) (div r c)

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

  (* (\* r = i*c => (i = r/c \/ c=r=0) *\)
   * let filter_mul_cst (i:t) (c:t) (r:t) : (t*t) bot =
   *   merge_bot2
   *     (if contains r B.zero && contains c B.zero then Nb i
   *     else match fst (div r c) with Bot -> Bot | Nb x -> meet i x)
   *     (Nb c)
   *
   * (\* r = i*c => (i = r/c \/ c=r=0) *\)
   * let filter_cst_mul (i:t) (c:t) (r:t) : (t*t) bot =
   *   merge_bot2
   *     (Nb c)
   *     (if contains r B.zero && contains c B.zero then Nb i
   *     else match fst (div r c) with Bot -> Bot | Nb x -> meet i x) *)

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
  let filter_sqrt ((il,ih) as i:t) ((rl,rh):t) : t bot =
    let rr = B.mul_down rl rl, B.mul_up rh rh in
    if B.sign il >= 0 then meet i rr
    else meet i (B.minus_inf, snd rr)

  (* r = exp i => i = ln r *)
  let filter_exp i r =
    meet_bot meet i (ln r)

  (* r = ln i => i = exp r *)
  let filter_ln i r =
    meet i (exp r)

  (* r = log i => i = *)
  let filter_log i r = failwith "todo filter_log"

  (* r = i ** n => i = nroot r *)
  let filter_pow (i:t) n (r:t) =
    (* let nri = n_root r n in *)
    (* let fnri = meet_bot meet i (n_root r n) in *)
    (* Format.printf "%s %s %s => %s => %s%!\n" (to_string i) (to_string r) (to_string n) (Bot.bot_to_string to_string nri) (Bot.bot_to_string to_string fnri); *)
    merge_bot2 (meet_bot meet i (n_root r n)) (Nb n)

  (* r = nroot i => i = r ** n *)
  let filter_root i r n =
     merge_bot2 (meet i (pow r n)) (Nb n)

  (* r = min (i1, i2) *)
  let filter_min (l1, u1) (l2, u2) (lr, ur) =
    merge_bot2 (check_bot ((B.max l1 lr), (B.max u1 ur))) (check_bot ((B.max l2 lr), (B.max u2 ur)))

  (* r = max (i1, i2) *)
  let filter_max (l1, u1) (l2, u2) (lr, ur) =
    merge_bot2 (check_bot ((B.min l1 lr), (B.min u1 ur))) (check_bot ((B.min l2 lr), (B.min u2 ur)))

  (* r = f(x0,x1,...,xn) *)
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


  let filter_bounds (l,h) =
    let inf = B.ceil l
    and sup = B.floor h in
    check_bot (inf, sup)

  let to_float_range (l,h) =
    (B.to_float_down l),(B.to_float_up h)

  let to_rational_range (l, h) =
    (B.to_rat l), (B.to_rat h)

  let to_range x = x

  (* returns the type annotation of the represented values *)
  let to_annot _ = Csp.Real

  (* generate a random float between l and h *)
  let spawn (l,h) =
    let r = Random.float 1. in
    let res = B.add_up l (B.mul_up (B.sub_up h l) (B.of_float_up r)) in
    B.to_float_up res

  let shrink ((l,h) : t) (c:Bound_rat.t) : t bot =
    try
      let c' = B.of_rat_up c in
      (B.add_up l c', B.sub_down h c')
        |> validate
        |> check_bot
    with Invalid_argument _ -> Bot
end

module ItvF = Itv(Bound_float)
module ItvQ = Itv(Bound_rat)
module ItvI = Itv(Bound_int)
