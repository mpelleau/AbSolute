(** Generic intervals. Can be instantiated with any bound type. An interval is a
    pair `(l,u)` where `l` is the lower bound and `u` is the upper bound.
    Therefore it does not handle "holes" in the domain. *)

open Bound_sig
open Tools

module Eval (B : BOUND) = struct
  (* interval bound (possibly -oo or +oo *)
  module B = B

  type bound = B.t

  (* an interval is a pair of bounds (lower,upper); intervals are always
     non-empty: lower <= upper; functions that can return an empty interval
     return it as Bot *)
  type t = bound * bound

  (* PRINTING *)

  let pp_print_bound fmt (b : B.t) =
    if B.ceil b = b then Format.fprintf fmt "%0F" (B.to_float_down b)
    else Format.fprintf fmt "%a" Format.pp_print_float (B.to_float_down b)

  (* printer *)
  let print fmt ((l, h) : t) =
    if l = h then pp_print_bound fmt l
    else Format.fprintf fmt "[%a;%a]" pp_print_bound l pp_print_bound h

  (* not all pairs of bounds are valid intervals *)
  let validate ((l, h) as i : t) : t =
    match (B.classify l, B.classify h) with
    | (B.INVALID, _ | _, B.INVALID | B.MINF, _ | _, B.INF | _) when B.gt l h ->
        invalid_fmt "int.validate : [%a; %a]" pp_print_bound l pp_print_bound h
    | _ -> i

  (* maps empty intervals to explicit bottom *)
  let check_bot ((l, h) as i : t) : t option =
    if B.leq l h then Some i else None

  let debot ((l, h) as i) = if B.leq l h then i else raise Bot_found

  (************************************************************************)
  (* CONSTRUCTORS AND CONSTANTS *)
  (************************************************************************)

  let of_bound (x : B.t) : t = validate (x, x)

  let zero : t = of_bound B.zero

  let one : t = of_bound B.one

  let minus_one : t = of_bound B.minus_one

  let top_real : t = (B.minus_inf, B.inf)

  let top_int : t = (B.minus_inf, B.inf)

  let zero_one : t = (B.zero, B.one)

  let minus_one_zero : t = (B.minus_one, B.zero)

  let minus_one_one : t = (B.minus_one, B.one)

  let positive : t = (B.zero, B.inf)

  let negative : t = (B.minus_inf, B.zero)

  let of_bounds (l : bound) (h : bound) = validate (l, h)

  let of_ints (l : int) (h : int) : t =
    of_bounds (B.of_int_down l) (B.of_int_up h)

  let of_int (x : int) = of_ints x x

  let of_rats (l : Mpqf.t) (h : Mpqf.t) : t =
    of_bounds (B.of_rat_down l) (B.of_rat_up h)

  let of_rat (x : Mpqf.t) = of_rats x x

  let of_floats (l : float) (h : float) : t =
    of_bounds (B.of_float_down l) (B.of_float_up h)

  let of_float (x : float) = of_floats x x

  let hull (x : B.t) (y : B.t) = (B.min x y, B.max x y)

  let to_constraint v ((l, h) : t) =
    Constraint.inside_cst v (B.to_rat l) (B.to_rat h)

  (* SET-THEORETIC *)

  let join ((l1, h1) : t) ((l2, h2) : t) : t = (B.min l1 l2, B.max h1 h2)

  let meet_opt ((l1, h1) as i1 : t) ((l2, h2) as i2 : t) : t option =
    if B.leq l1 l2 then if B.geq h1 h2 then Some i2 else check_bot (l2, h1)
    else if B.geq h2 h1 then Some i1
    else check_bot (l1, h2)

  let meet ((l1, h1) as i1 : t) ((l2, h2) as i2 : t) : t =
    if B.leq l1 l2 then if B.geq h1 h2 then i2 else debot (l2, h1)
    else if B.geq h2 h1 then i1
    else debot (l1, h2)

  let negative_part ((l1, h1) : t) : t option =
    if B.geq l1 B.zero then None else check_bot (l1, B.min h1 B.zero)

  let positive_part ((l1, h1) : t) : t option =
    if B.leq h1 B.zero then None else check_bot (B.max l1 B.zero, h1)

  (* predicates *)
  (* ---------- *)

  let equal ((l1, h1) : t) ((l2, h2) : t) : bool =
    B.equal l1 l2 && B.equal h1 h2

  let subseteq ((l1, h1) : t) ((l2, h2) : t) : bool = B.geq l1 l2 && B.leq h1 h2

  let contains ((l, h) : t) (x : B.t) : bool = B.leq l x && B.leq x h

  let intersect ((l1, h1) : t) ((l2, h2) : t) : bool =
    B.leq l1 h2 && B.leq l2 h1

  let is_finite x = B.classify x = B.FINITE

  let is_bounded ((l, h) : t) = is_finite l && is_finite h

  let is_singleton ((l, h) : t) : bool = is_finite l && B.equal l h

  let contains_float ((l, h) : t) (x : float) : bool =
    B.leq l (B.of_float_down x) && B.leq (B.of_float_up x) h

  let is_positive (l, _) = B.geq l B.zero

  let is_negative (_, h) = B.leq h B.zero

  let to_float_range (l, h) = (B.to_float_down l, B.to_float_up h)

  let to_rational_range (l, h) = (B.to_rat l, B.to_rat h)

  (* returns the type annotation of the represented values *)
  let to_annot _ = Csp.Real

  (* generate a random float between l and h *)
  let spawn (l, h) =
    let r = Random.float 1. in
    let res = B.add_up l (B.mul_up (B.sub_up h l) (B.of_float_up r)) in
    B.to_float_up res

  (* mesure *)
  (* ------ *)

  (* length of the intersection (>= 0) *)
  let overlap ((l1, h1) : t) ((l2, h2) : t) : B.t =
    B.max B.zero (B.sub_up (B.min h1 h2) (B.max l1 l2))

  let range ((l, h) : t) : B.t = B.sub_up h l

  let float_size ((l, h) : t) : float = B.to_float_up (B.sub_up h l)

  (* split *)
  (* ----- *)

  (* split priority *)
  let score itv = float_size itv

  (* find the mean of the interval; when a bound is infinite, then "mean" is
     some value strictly inside the interval *)
  let mean ((l, h) : t) : B.t =
    match (is_finite l, is_finite h) with
    | true, true ->
        (* finite bounds: either split on 0 to esure monotony, or return the
           actual mean *)
        if B.lt l B.zero && B.gt h B.zero then B.zero
        else B.div_up (B.sub_up h l) B.two |> B.add_down l
    | true, false ->
        (* [l,+oo] *)
        if B.sign l < 0 then B.zero (* cut at 0 if [l,+oo] contains 0 *)
        else if B.sign l = 0 then B.one (* cut at 1 if [l,+oo] touches 0 *)
        else B.mul_up l B.two (* cut at 2l if [l,+oo] is positive *)
    | false, true ->
        (* [-oo,h]: similar to [l,+oo] *)
        if B.sign h > 0 then B.zero
        else if B.sign h = 0 then B.minus_one
        else B.mul_down h B.two
    | false, false ->
        (* the mean of [-oo,+oo] is 0 *)
        B.zero

  let split_on_value ((l, h) : t) (x : B.t) : t list =
    let rec aux acc cur bounds =
      match bounds with
      | hd :: tl ->
          let itv = validate (cur, hd) in
          aux (itv :: acc) hd tl
      | [] ->
          let itv = validate (cur, h) in
          itv :: acc
    in
    aux [] l [x]

  (* splits in two, around the middle *)
  let split (i : t) : t list = split_on_value i (mean i)

  (* there's no exact difference operator on itv with closed bounds *)
  let prune = None

  (************************************************************************)
  (* INTERVAL ARITHMETICS (FORWARD EVALUATION) *)
  (************************************************************************)

  let neg ((l, h) : t) : t = (B.neg h, B.neg l)

  let abs ((l, h) : t) : t =
    if contains (l, h) B.zero then (B.zero, B.max (B.abs l) (B.abs h))
    else hull (B.abs l) (B.abs h)

  let add ((l1, h1) : t) ((l2, h2) : t) : t = (B.add_down l1 l2, B.add_up h1 h2)

  let sub ((l1, h1) : t) ((l2, h2) : t) : t = (B.sub_down l1 h2, B.sub_up h1 l2)

  let bound_mul_up = B.bound_mul B.mul_up

  let bound_mul_down = B.bound_mul B.mul_down

  let mix4 up down ((l1, h1) : t) ((l2, h2) : t) =
    ( B.min (B.min (down l1 l2) (down l1 h2)) (B.min (down h1 l2) (down h1 h2))
    , B.max (B.max (up l1 l2) (up l1 h2)) (B.max (up h1 l2) (up h1 h2)) )

  let mul = mix4 bound_mul_up bound_mul_down

  let bound_div_up = B.bound_div B.div_up

  let bound_div_down = B.bound_div B.div_down

  (* helper: assumes i2 has constant sign *)
  let div_sign ((l1, h1) as i1) ((l2, h2) as i2) =
    if B.sign h2 = 0 then
      (B.minus_inf, B.max (bound_div_up l1 l2) (bound_div_up h1 l2))
    else if B.sign l2 = 0 then
      (B.min (bound_div_down l1 h2) (bound_div_down h1 h2), B.inf)
    else mix4 bound_div_up bound_div_down i1 i2

  (* return valid values (possibly Bot) + possible division by zero *)
  let div (i1 : t) (i2 : t) : t option =
    (* split into positive and negative dividends *)
    let pos = (Option.map (div_sign i1)) (positive_part i2)
    and neg = (Option.map (div_sign i1)) (negative_part i2) in
    (* joins the result*)
    Tools.join_bot2 join pos neg

  (* interval square root *)
  let sqrt ((l, h) : t) : t option =
    if B.sign h < 0 then None
    else
      let l = B.max l B.zero in
      Some (B.sqrt_down l, B.sqrt_up h)

  (* useful operators on intervals *)
  let ( +@ ) = add

  let ( -@ ) = sub

  let ( *@ ) = mul

  let ( /@ ) = div

  let ln10 = B.of_float_up 2.3025850

  (* interval exp *)
  let exp (l, h) = (B.exp_down l, B.exp_up h)

  (* interval ln *)
  let ln (l, h) =
    if B.leq h B.zero then None
    else if B.leq l B.zero then Some (B.minus_inf, B.ln_up h)
    else Some (B.ln_down l, B.ln_up h)

  (* interval log *)
  let log itv = Option.bind (ln itv) (fun x -> div x (of_bound ln10))

  let force_int (l, h) =
    if B.floor l = l && h = l then B.to_float_down l |> int_of_float
    else failwith "should be a singleton integer"

  (* powers *)
  let pow ((il, ih) : t) (exp : t) =
    let p = force_int exp in
    match p with
    | 0 -> one
    | 1 -> (il, ih)
    | x ->
        if x > 1 then
          if x mod 2 = 1 then (B.pow_down il p, B.pow_up ih p)
          else if B.leq il B.zero && B.geq ih B.zero then
            (B.zero, B.max (B.pow_up il p) (B.pow_up ih p))
          else if B.geq il B.zero then (B.pow_down il p, B.pow_up ih p)
          else (B.pow_down ih p, B.pow_up il p)
        else failwith "cant handle negatives powers"

  (* nth-root *)
  let n_root ((il, ih) : t) (exp : t) =
    let p = force_int exp in
    match p with
    | 1 -> Some (il, ih)
    | x when x > 1 && p mod 2 = 1 -> Some (B.root_down il p, B.root_up ih p)
    | x when x > 1 ->
        if B.lt ih B.zero then None
        else if B.leq il B.zero then
          Some (B.neg (B.root_up ih p), B.root_up ih p)
        else
          Some
            ( B.min (B.neg (B.root_down il p)) (B.neg (B.root_down ih p))
            , B.max (B.root_up il p) (B.root_up ih p) )
    | _ -> failwith "can only handle stricly positive roots"

  (* interval min *)
  let min ((l1, u1) : t) ((l2, u2) : t) = (B.min l1 l2, B.min u1 u2)

  (* interval max *)
  let max ((l1, u1) : t) ((l2, u2) : t) = (B.max l1 l2, B.max u1 u2)

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
    | "max" -> arity_2 max
    | "min" -> arity_2 min
    | s -> failwith (Format.sprintf "unknown eval function : %s" s)
end

module Itv (B : BOUND) = struct
  module E = Eval (B)
  include E
  include Filter.Make (E)

  (* guards *)

  let filter_leq ((l1, h1) : t) ((l2, h2) : t) : (t * t) Consistency.t =
    let open Consistency in
    if B.leq h1 l2 then Sat
    else if B.gt l1 h2 then Unsat
    else Filtered (((l1, B.min h1 h2), (B.max l1 l2, h2)), l1 = h1 || l2 = h2)

  let filter_lt ((l1, h1) as i1 : t) ((l2, h2) as i2 : t) :
      (t * t) Consistency.t =
    let open Consistency in
    if B.lt h1 l2 then Sat
    else if is_singleton i1 && i1 = i2 then Unsat
    else Filtered (((l1, B.min h1 h2), (B.max l1 l2, h2)), false)

  let filter_eq ((l1, h1) as i1 : t) ((l2, h2) as i2 : t) : t Consistency.t =
    let open Consistency in
    if is_singleton i1 && i1 = i2 then Sat
    else
      let l = B.max l1 l2 and u = B.min h1 h2 in
      if B.leq l u then Filtered ((l, u), false) else Unsat

  let filter_neq ((l1, h1) as i1 : t) ((l2, h2) as i2 : t) :
      (t * t) Consistency.t =
    let open Consistency in
    if B.equal l1 h1 && B.equal l2 h2 && B.equal l1 l2 then Unsat
    else
      let l = B.max l1 l2 and u = B.min h1 h2 in
      if B.leq l u then Filtered ((i1, i2), false) else Sat
end

module ItvF = Itv (Bound_float)
module ItvQ = Itv (Bound_mpqf)
