(* We encode -infinity and infinity as `-1/0` and `1/0`. *)
type t = Mpqf.t

(* ordering *)

let name = "rational"
let is_continuous = true
let succ x = x
let prec x = x

(* useful constants *)
let zero : t = Mpqf.of_int 0
let one : t = Mpqf.of_int 1
let two : t = Mpqf.of_int 2
let minus_one : t = Mpqf.of_int (-1)
let minus_two : t = Mpqf.of_int (-2)
let inf : t = Mpqf.of_frac 1 0
let minus_inf : t = Mpqf.of_frac (-1) 0
let rat_nan : t = Mpqf.of_frac 0 0

(* classification *)
type kind = FINITE | MINF | INF | INVALID

let classify (x:t) : kind =
  let (num, den) = Mpqf.to_mpzf2 x in
  match (Mpzf.sgn num, Mpzf.sgn den) with
    | 0, 0 -> INVALID
    | 1, 0 -> INF
    | -1, 0 -> MINF
    | _ -> FINITE

(* Comparison functions. *)
(* For NAN comparison, we try to follow the same rules as for floating point numbers. *)

(* `c` is what to return if one argument is `nan`. *)
let compare' a b c =
  let rec aux = function
  | INVALID, _ -> c
  | _, INVALID -> c
  | FINITE, FINITE -> Mpqf.cmp a b
  | INF, INF -> 0
  | INF, _ -> 1
  | MINF, MINF -> 0
  | MINF, _ -> -1
  | a', b' -> -(aux (b', a')) in
  aux ((classify a), (classify b))

let compare (a:t) (b:t) = compare' a b (-1)
let equal (a:t) (b:t) = (compare' a b 1) = 0
let leq (x:t) (y:t) : bool = (compare' x y 1) <= 0
let geq (x:t) (y:t) : bool = (compare' x y (-1)) >= 0
let lt (x:t) (y:t) : bool = (compare' x y 0) < 0
let gt (x:t) (y:t) : bool = (compare' x y 0) > 0
let neq (x:t) (y:t) : bool = not (equal x y)

let min (x:t) (y:t) : t = if (compare x y) <= 0 then x else y
let max (x:t) (y:t) : t = if (compare x y) >= 0 then x else y

let sign (x:t) : int =
    match classify x with
  | FINITE -> Mpqf.sgn x
  | INF -> 1
  | MINF -> -1
  | INVALID -> 0

let odd (x:t) : bool = ((int_of_float (Mpqf.to_float x)) / 2) * 2 |> float <>  (Mpqf.to_float x)
let even (x:t) : bool = ((int_of_float (Mpqf.to_float x)) / 2) * 2 |> float =  (Mpqf.to_float x)

(* conversion, printing *)
(* Is equal to Bound_int.inf and Bound_int.minus_inf.
   We cannot use them directly otherwise we have a circular dependency.*)
let int_inf = max_int
let int_minus_inf = min_int
let of_int a =
  if a = int_inf then inf
  else if a = int_minus_inf then minus_inf
  else Mpqf.of_int a
let of_int_up a = of_int a
let of_int_down a = of_int a

let of_float a =
  if a = infinity then inf
  else if a = neg_infinity then minus_inf
  else if a = nan then rat_nan
  else Mpqf.of_float a
let of_float_up a : t = of_float a
let of_float_down a : t = of_float a
let of_rat_up a : t = a
let of_rat_down a : t = a

let of_frac a b = Mpqf.of_frac a b

let of_string x = Mpqf.of_string x

(* TODO *)
let of_string_up = of_string
let of_string_down = of_string

(* This is a helper function that apply `f` to `x` only for finite value.*)
let wrap f rinf rminf rnan x =
  match classify x with
  | FINITE -> f x
  | INF -> rinf
  | MINF -> rminf
  | INVALID -> rnan

let to_string x = wrap Mpqf.to_string "inf" "-inf" "nan" x

let of_mpqf a : t = a
let to_mpqf x : Mpqf.t = x

let to_float_up x : float =
  wrap Mpqf.to_float infinity neg_infinity nan x

let to_float_down x : float =
  wrap (fun x -> -.(Mpqf.to_float (Mpqf.neg x))) infinity neg_infinity nan x

let to_rat x = x

let to_int_up x = (int_of_float (ceil (to_float_up x)))
let to_int_down x = (int_of_float (floor (to_float_down x)))

(* printing *)
let output chan x = output_string chan (to_string x)
let sprint () x = to_string x
let bprint b x = Buffer.add_string b (to_string x)
let pp_print f x = Format.pp_print_string f (to_string x)

(* exact operators *)
let neg x = wrap Mpqf.neg minus_inf inf rat_nan x
let abs x = wrap Mpqf.abs inf inf rat_nan x

(* operators with rounding *)
let wrap2 f f' x y =
  match classify x, classify y with
  | FINITE, FINITE -> f x y
  | INVALID, _ -> rat_nan
  | _, INVALID -> rat_nan
  | _, _ -> (of_float (f' (to_float_up x) (to_float_up y)))

let add a b = wrap2 Mpqf.add (+.) a b
let sub a b = wrap2 Mpqf.sub (-.) a b
let mul a b = wrap2 Mpqf.mul ( *.) a b
let div a b = wrap2 Mpqf.div (/.) a b

let add_up = add
let sub_up = sub
let mul_up = mul
let div_up = div

let add_down = add
let sub_down = sub
let mul_down = mul
let div_down = div

(* helper: oo * 0 = 0 when multiplying bounds *)
let bound_mul f x y =
  if sign x = 0 || sign y = 0 then zero else f x y

(* helper: 0/0 = 0, x/0 = sign(x) oo *)
let bound_div f x y =
  match sign x, sign y with
  |  0,_ -> zero
  |  1,0 -> inf
  | -1,0 -> minus_inf
  | _ -> f x y

(* TODO: improve and check soundness *)
let sqrt_up x = of_float (sqrt (to_float_up x))
let sqrt_down x = of_float (-. ((-. 1.) /. (sqrt (1. /. (to_float_up x)))))

let cos_up x = of_float (cos (to_float_up x))
let cos_down x = of_float (cos (to_float_up x))

let sin_up x = of_float (sin (to_float_up x))
let sin_down x = of_float (sin (to_float_up x))

let tan_up x = of_float (tan (to_float_up x))
let tan_down x = of_float (tan (to_float_up x))

let acos_up x = of_float (acos (to_float_up x))
let acos_down x = of_float (acos (to_float_up x))

let asin_up x = of_float (asin (to_float_up x))
let asin_down x = of_float (asin (to_float_up x))

let atan_up x = of_float (atan (to_float_up x))
let atan_down x = of_float (atan (to_float_up x))

let exp_up x = of_float (exp (to_float_up x))
let exp_down x = of_float (exp (to_float_up x))

let ln_up x = of_float (log (to_float_up x))
let ln_down x = of_float (log (to_float_up x))

let log_up x = of_float (log10 (to_float_up x))
let log_down x = of_float (log10 (to_float_up x))

let floor x = of_float (floor (to_float_up x))
let ceil x = of_float (ceil (to_float_up x))

let pow_up x n = of_float ((to_float_up x) ** (float n))
let pow_down x n =  of_float (-. ((-. (to_float_up x)) ** (float n)))
let root_up x n =  of_float (exp((log (to_float_up x)) /. (float n)))
let root_down x n =  of_float (exp((log (to_float_up x)) /. (float n)))
