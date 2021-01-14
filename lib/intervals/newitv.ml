open Bound_sig

module Eval (B : BOUND) = struct
  module B = B

  type bound = B.t

  type kind = Strict | Large

  type real_bound = kind * bound

  let sym = function Large, x -> (Strict, x) | Strict, x -> (Large, x)

  let stricten = function Large, x -> (Strict, x) | x -> x

  let mix k1 k2 = match (k1, k2) with Large, Large -> Large | _ -> Strict

  let bound_arith f ((k1, b1) : real_bound) ((k2, b2) : real_bound) =
    (mix k1 k2, f b1 b2)

  let bound_mul_up = B.bound_mul B.mul_up

  let bound_mul_down = B.bound_mul B.mul_down

  let bound_div_up = B.bound_div B.div_up

  let bound_div_down = B.bound_div B.div_down

  let ( +@ ) = bound_arith B.add_up

  let ( +$ ) = bound_arith B.add_down

  let ( -@ ) = bound_arith B.sub_up

  let ( -$ ) = bound_arith B.sub_down

  let ( *@ ) = bound_arith bound_mul_up

  let ( *$ ) = bound_arith bound_mul_down

  let ( /@ ) = bound_arith bound_div_up

  let ( /$ ) = bound_arith bound_div_down

  type t = real_bound * real_bound

  (* returns the half space defined by a bound and a direction. - true for going
     toward +oo - false for going toward -oo Ex: half_space (Strict,0) true
     gives ]0; +oo[ *)
  let half_space (k, b) dir =
    ( match (k, dir) with
    | Strict, false -> B.gt
    | Strict, true -> B.lt
    | Large, false -> B.geq
    | Large, true -> B.leq )
      b

  (* check if a value is in a half space *)
  let in_half (k, b) dir v : bool = v |> half_space (k, b) dir

  let cmp_low ((_, b1) as l1) ((_, b2) as l2) =
    if in_half l1 true b2 then -1 else if in_half l2 true b1 then 1 else 0

  (* compare two up bounds *)
  let cmp_up ((_, b1) as u1) ((_, b2) as u2) =
    if in_half u1 false b2 then 1 else if in_half u2 false b1 then -1 else 0

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
  let check_bot ((((_, l) as b1), ((_, h) as b2)) as itv) : t option =
    if in_half b1 true h && in_half b2 false l then Some itv else None

  (* not all pairs of rationals are valid intervals *)
  let validate x = if check_bot x = None then failwith "invalid interval" else x

  (************************************************************************)
  (* CONSTRUCTORS AND CONSTANTS *)
  (************************************************************************)

  let large (x : B.t) (y : B.t) : t = validate ((Large, x), (Large, y))

  let strict (x : B.t) (y : B.t) : t = validate ((Strict, x), (Strict, y))

  let large_strict (x : B.t) (y : B.t) : t = validate ((Large, x), (Strict, y))

  let strict_large (x : B.t) (y : B.t) : t = validate ((Strict, x), (Large, y))

  let of_bound (x : B.t) : t = large x x

  let zero : t = of_bound B.zero

  let one : t = of_bound B.one

  let minus_one : t = of_bound B.minus_one

  let top_real : t = ((Strict, B.minus_inf), (Strict, B.inf))

  let top_int : t = ((Strict, B.minus_inf), (Strict, B.inf))

  let top = top_real

  let zero_one : t = large B.zero B.one

  let minus_one_zero : t = large B.minus_one B.zero

  let minus_one_one : t = large B.minus_one B.one

  let positive : t = large_strict B.zero B.inf

  let negative : t = strict_large B.minus_inf B.zero

  let of_bounds = large

  let of_ints (l : int) (h : int) : t =
    of_bounds (B.of_int_down l) (B.of_int_up h)

  let of_int (x : int) : t = of_ints x x

  let of_floats (l : float) (h : float) : t =
    of_bounds (B.of_float_down l) (B.of_float_up h)

  let of_float (x : float) : t = of_floats x x

  let of_rats (l : Mpqf.t) (h : Mpqf.t) : t =
    of_bounds (B.of_rat_down l) (B.of_rat_up h)

  let of_rat (x : Mpqf.t) = of_rats x x

  let hull (x : B.t) (y : B.t) : t = try large x y with Failure _ -> large y x

  let half_sup (x : B.t) : t = large_strict x B.inf

  let half_inf (x : B.t) : t = strict_large B.minus_inf x

  (************************************************************************)
  (* PRINTING *)
  (************************************************************************)
  let to_string (((kl, l), (kh, h)) : t) : string =
    Printf.sprintf "%c%a;%a%c"
      (if kl = Strict then ']' else '[')
      B.sprint l B.sprint h
      (if kh = Large then ']' else '[')

  let print fmt (x : t) = Format.fprintf fmt "%s" (to_string x)

  let to_bexpr v (((kl, l), (kh, h)) : t) =
    let v = Csp.Var v in
    let l_cst = Csp.Cst (B.to_rat l) and h_cst = Csp.Cst (B.to_rat h) in
    Csp.(
      And
        ( Cmp (v, (match kl with Strict -> GT | Large -> GEQ), l_cst)
        , Cmp (v, (match kh with Strict -> LT | Large -> LEQ), h_cst) ))

  (************************************************************************)
  (* SET-THEORETIC *)
  (************************************************************************)

  (* operations *)
  (* ---------- *)
  let join ((l1, h1) : t) ((l2, h2) : t) : t = (min_low l1 l2, max_up h1 h2)

  let meet ((l1, h1) : t) ((l2, h2) : t) : t option =
    check_bot (max_low l1 l2, min_up h1 h2)

  (* ---------- *)
  (* predicates *)
  (* ---------- *)
  let equal ((l1, h1) : t) ((l2, h2) : t) : bool =
    let equal_bound (k1, b1) (k2, b2) = k1 = k2 && B.equal b1 b2 in
    equal_bound l1 l2 && equal_bound h1 h2

  (* i1 in i2*)
  let subseteq i1 i2 : bool = join i1 i2 |> equal i2

  let contains ((l, h) : t) (x : B.t) : bool =
    in_half l true x && in_half h false x

  let contains_float (i : t) (x : float) : bool = contains i (B.of_float_up x)

  (*TODO: improve rounding errors resiliance *)

  let intersect i1 i2 : bool = meet i1 i2 <> None

  let is_finite ((_, x) : real_bound) : bool = B.classify x = B.FINITE

  let is_bounded ((l, h) : t) = is_finite l && is_finite h

  let is_singleton ((l, h) : t) : bool = is_finite l && B.equal (snd l) (snd h)

  let range (((_, l), (_, h)) : t) = B.sub_up h l

  let float_size (i : t) : float = B.to_float_up (range i)

  let is_positive (((_, l), _) : t) = B.geq l B.zero

  let is_negative ((_, (_, h)) : t) = B.leq h B.zero

  (* split *)
  (* ----- *)

  (* split priority *)
  let score itv = float_size itv

  (* length of the intersection (>= 0) *)
  let overlap i1 i2 = match meet i1 i2 with None -> B.zero | Some i -> range i

  let magnitude (((_, l), (_, h)) : t) : B.t = B.max (B.abs l) (B.abs h)

  let mean ((((_, l) as low), ((_, h) as high)) : t) : B.t =
    match (is_finite low, is_finite high) with
    | true, true -> B.div_up (B.add_up l h) B.two
    | true, false ->
        if B.sign l < 0 then B.zero
        else if B.sign l = 0 then B.one
        else B.mul_up l B.two
    | false, true ->
        if B.sign h > 0 then B.zero
        else if B.sign h = 0 then B.minus_one
        else B.mul_down h B.two
    | false, false -> B.zero

  let split_on_value ((l, h) : t) (x : B.t) : t list =
    let rec aux acc cur bounds =
      match bounds with
      | hd :: tl ->
          let itv = validate (cur, (Large, hd)) in
          aux (itv :: acc) (Strict, hd) tl
      | [] ->
          let itv = validate (cur, h) in
          itv :: acc
    in
    aux [] l [x]

  (* splits in two, around the middle *)
  let split (i : t) : t list = split_on_value i (mean i)

  let prune ((l, h) : t) ((l', h') : t) : t list =
    match (gt_low l' l, lt_up h' h) with
    | true, true -> [(l, sym l'); (sym h', h)]
    | true, false -> [(l, sym l')]
    | false, true -> [(sym h', h)]
    | false, false -> []

  let prune = Some prune

  (************************************************************************)
  (* INTERVAL ARITHMETICS (FORWARD EVALUATION) *)
  (************************************************************************)

  (* few utilities based on monotony *)
  (***********************************)

  (* f [a;b] -> [f(a);f(b)] when f is monotonic increasing *)
  let mon_incr (f_down, f_up) ((kl, low), (kh, high)) =
    ((kl, f_down low), (kh, f_up high))

  (* f [a;b] -> [f(b);f(a)] when f is monotonic decreasing *)
  let mon_decr (f_down, f_up) ((kl, low), (kh, high)) =
    ((kh, f_down high), (kl, f_up low))

  (* when f changes its monotony once in "c"; - if f ↗↘ then f [a;b] =
     [f(a);f(c)] U [f(b);f(c)] - if f ↘↗ then f [a;b] = [f(c);f(a)] U
     [f(c);f(b)] *)
  let mon_2 f c ((a, b) as itv) first =
    let f1, f2 = if first then (mon_incr, mon_decr) else (mon_decr, mon_incr) in
    if subseteq itv (half_inf c) then f1 f itv
    else if subseteq itv (half_sup c) then f2 f itv
    else join (f1 f (a, (Large, c))) (f2 f ((Large, c), b))

  (* arithmetical functions *)
  (**************************)

  let neg (i : t) : t = mon_decr (B.neg, B.neg) i

  let abs (i : t) : t =
    let f = (B.abs, B.abs) in
    if subseteq i positive then mon_incr f i
    else if subseteq i negative then mon_decr f i
    else mon_2 (B.abs, B.abs) B.zero i false

  let add ((l1, h1) : t) ((l2, h2) : t) : t = (l1 +$ l2, h1 +@ h2)

  let sub ((l1, h1) : t) ((l2, h2) : t) : t = (l1 -$ h2, h1 -@ l2)

  let mul ((l1, h1) : t) ((l2, h2) : t) : t =
    ( min_low (min_low (l1 *$ l2) (l1 *$ h2)) (min_low (h1 *$ l2) (h1 *$ h2))
    , max_up (max_up (l1 *@ l2) (l1 *@ h2)) (max_up (h1 *@ l2) (h1 *@ h2)) )

  let div_sgn ((l1, h1) : t) ((l2, h2) : t) : t =
    if B.sign (snd h2) = 0 then
      ((Large, B.minus_inf), max_up (l1 /@ l2) (h1 /@ l2))
    else if B.sign (snd l2) = 0 then
      (min_low (l1 /$ h2) (h1 /$ h2), (Large, B.inf))
    else
      ( min_low (min_low (l1 /$ l2) (l1 /$ h2)) (min_low (h1 /$ l2) (h1 /$ h2))
      , max_up (max_up (l1 /@ l2) (l1 /@ h2)) (max_up (h1 /@ l2) (h1 /@ h2)) )

  let div (i1 : t) (i2 : t) : t option =
    let pos = (Option.map (div_sgn i1)) (meet i2 positive) in
    let neg = (Option.map (div_sgn i1)) (meet i2 negative) in
    Tools.join_bot2 join pos neg

  let sqrt (itv : t) : t option =
    meet itv positive |> Option.map (mon_incr (B.sqrt_down, B.sqrt_up))

  let force_int ((sl, l), (sh, h)) =
    if sl = Large && B.floor l = l && sh = sl && h = l then
      B.to_float_down l |> int_of_float
    else failwith "should be a singleton integer"

  let pow (itv : t) (exp : t) =
    let i = force_int exp in
    let f_down_up = ((fun b -> B.pow_down b i), fun b -> B.pow_up b i) in
    let pow_odd x : t = mon_incr f_down_up x
    and pow_even x : t = mon_2 f_down_up B.zero x false in
    match i with
    | 0 -> one
    | 1 -> itv
    | x when x > 1 && i mod 2 = 1 -> pow_odd itv
    | x when x > 1 -> pow_even itv
    | _ -> failwith "cant handle negatives powers"

  (* nth-root *)
  let n_root (itv : t) (exp : t) : t option =
    let i = force_int exp in
    let f_down_up = ((fun b -> B.root_down b i), fun b -> B.root_up b i) in
    let root_odd x : t = mon_incr f_down_up x
    and root_even x : t option =
      meet x positive
      |> Option.map (fun x ->
             let pos_part = mon_incr f_down_up x in
             join pos_part (neg pos_part))
    in
    match i with
    | 1 -> Some itv
    | x when x > 1 && i mod 2 = 1 -> Some (root_odd itv)
    | x when x > 1 -> root_even itv
    | _ -> failwith "can only handle stricly positive roots"

  let ln (i : t) : t option =
    meet i positive |> Option.map (mon_incr (B.ln_down, B.ln_up))

  let exp (i : t) = mon_incr (B.exp_down, B.exp_up) i

  (*the two closest floating boundaries of pi*)
  let ln10_up = B.ln_up (B.of_int_up 10)

  let ln10_down = B.ln_down (B.of_int_down 10)

  (* it improves soundness to use those *)
  let i_ln10 : t = ((Large, ln10_down), (Large, ln10_up))

  (* interval log *)
  let log itv = Option.bind (ln itv) (fun i -> div i i_ln10)

  (* interval min *)
  let min (((kl1, l1), (kh1, h1)) : t) (((kl2, l2), (kh2, h2)) : t) : t =
    let low =
      if l1 < l2 then (kl1, l1)
      else if l2 < l1 then (kl2, l2)
      else (* l1 = l2 *) ((if kl1 = Large then kl1 else kl2), l1)
    and high =
      if h1 < h2 then (kh1, h1)
      else if h2 < h1 then (kh2, h2)
      else (* h1 = h2 *) ((if kh1 = Strict then kh1 else kh2), h2)
    in
    (low, high)

  (* interval max *)
  let max (((kl1, l1), (kh1, h1)) : t) (((kl2, l2), (kh2, h2)) : t) : t =
    let low =
      if l1 > l2 then (kl1, l1)
      else if l2 > l1 then (kl2, l2)
      else (* l1 = l2 *) ((if kl1 = Strict then kl1 else kl2), l1)
    and high =
      if h1 > h2 then (kh1, h1)
      else if h2 > h1 then (kh2, h2)
      else (* h1 = h2 *) ((if kh1 = Large then kh1 else kh2), h2)
    in
    (low, high)

  (** runtime functions **)
  let eval_fun name args : t option =
    let arity_1 (f : t -> t) : t option =
      match args with
      | [i] -> Some (f i)
      | _ -> failwith (Format.sprintf "%s expect one argument" name)
    in
    let arity_1_bot (f : t -> t option) : t option =
      match args with
      | [i] -> f i
      | _ -> failwith (Format.sprintf "%s expect one argument" name)
    in
    let arity_2 (f : t -> t -> t) : t option =
      match args with
      | [i1; i2] -> Some (f i1 i2)
      | _ -> failwith (Format.sprintf "%s expect two arguments" name)
    in
    let arity_2_bot (f : t -> t -> t option) : t option =
      match args with
      | [i1; i2] -> f i1 i2
      | _ -> failwith (Format.sprintf "%s expect two arguments" name)
    in
    match name with
    | "pow" -> arity_2 pow
    | "nroot" -> arity_2_bot n_root
    | "sqrt" -> arity_1_bot sqrt
    | "exp" -> arity_1 exp
    | "ln" -> arity_1_bot ln
    | "log" -> arity_1_bot log
    (* min max *)
    | "max" -> arity_2 max
    | "min" -> arity_2 min
    | s -> failwith (Format.sprintf "unknown eval function : %s" s)

  let to_float_range ((_, l), (_, h)) = (B.to_float_down l, B.to_float_up h)

  let to_rational_range ((_, l), (_, h)) = (B.to_rat l, B.to_rat h)

  (* returns the type annotation of the represented values *)
  let to_annot _ = Csp.Real

  (* generate a random float between l and h *)
  let spawn (((_, l), (_, h)) : t) =
    (* we convert toward the inside of the itv to avoid false negative *)
    let r = Random.float 1. in
    let res = B.add_up l (B.mul_up (B.sub_up h l) (B.of_float_up r)) in
    B.to_float_up res
end

module Make (B : BOUND) = struct
  module E = Eval (B)
  include E
  include Filter.Make (E)

  (* FILTERING (TEST TRANSFER FUNCTIONS) *)

  let filter_leq ((((_, vl1) as l1), ((_, vh1) as h1)) as i1 : t)
      ((((_, vl2) as l2), ((_, vh2) as h2)) as i2 : t) : (t * t) Consistency.t =
    let open Consistency in
    if B.leq vh1 vl2 then Sat
    else if B.gt vl1 vh2 then Unsat
    else
      Filtered
        ( ((l1, min_up h1 h2), (max_low l1 l2, h2))
        , is_singleton i1 || is_singleton i2 )

  let filter_lt ((((_, vl1) as l1), ((kh1, vh1) as h1)) as i1 : t)
      ((((kl2, vl2) as l2), ((_, vh2) as h2)) as i2 : t) : (t * t) Consistency.t
      =
    let open Consistency in
    if B.lt vh1 vl2 || (B.equal vh1 vl2 && (kh1 = Strict || kl2 = Strict)) then
      Sat
    else if B.geq vl1 vh2 then Unsat
    else
      Filtered
        ( ((l1, min_up h1 h2), (max_low l1 l2, h2))
        , is_singleton i1 || is_singleton i2 )

  let filter_eq ((l1, h1) as i1 : t) ((l2, h2) as i2 : t) : t Consistency.t =
    let open Consistency in
    if is_singleton i1 && is_singleton i2 && equal i1 i2 then Sat
    else
      let ((_, vl) as l) = max_low l1 l2 and ((_, vh) as h) = min_up h1 h2 in
      if in_half l true vh && in_half h false vl then Filtered ((l, h), false)
      else Unsat

  let filter_neq ((l1, h1) as i1 : t) ((l2, h2) as i2 : t) :
      (t * t) Consistency.t =
    let open Consistency in
    if is_singleton i1 && is_singleton i2 && equal i1 i2 then Unsat
    else
      let ((_, vl) as l) = max_low l1 l2 and ((_, vh) as h) = min_up h1 h2 in
      if in_half l true vh && in_half h false vl then
        let r = (l, h) in
        Filtered ((r, r), false)
      else Sat
end

module Test = Make (Bound_float)
module TestQ = Make (Bound_mpqf)
