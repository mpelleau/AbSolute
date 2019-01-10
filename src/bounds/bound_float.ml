(* (\* sets FPU rounding mode towards +oo, once and for all *\) *)
external init: unit -> unit = "ml_float_init"
let _ = init ()

type t = float


(* ordering *)

let compare (a:t) (b:t) = compare a b
let equal (a:t) (b:t) = a = b
let leq (x:t) (y:t) : bool = x <= y
let geq (x:t) (y:t) : bool = x >= y
let lt (x:t) (y:t) : bool = x < y
let gt (x:t) (y:t) : bool = x > y
let neq (x:t) (y:t) : bool = x <> y

let odd (x:t) : bool = ((int_of_float x) / 2) * 2 |> float <> x
let even (x:t) : bool = ((int_of_float x) / 2) * 2 |> float = x

let min (x:t) (y:t) : t = min x y
let max (x:t) (y:t) : t = max x y

let sign (x:t) : int =
  if x > 0. then 1 else
  if x < 0. then -1 else 0

(* conversion, printing *)

let of_int_up a = float_of_int a
let of_int_down a = -. (float_of_int (-a))
let of_float_up a : t = a
let of_float_down a : t = a
let of_rat_down = Bound_rat.to_float_down
let of_rat_up = Bound_rat.to_float_up

let of_string x =
  float_of_string x

(* TODO *)
let of_string_up = of_string
let of_string_down = of_string

(* Note: adds 0. to favor positive 0 *)
let to_string x = string_of_float (x+.0.)

let to_float_up x : float = x
let to_float_down x : float = x
let to_rat x = Bound_rat.of_float x

(* printing *)
let output chan x = output_string chan (to_string x)
let sprint () x = to_string x
let bprint b x = Buffer.add_string b (to_string x)
let pp_print f x = Format.pp_print_string f (to_string x)


(* classification *)

type kind = FINITE | MINF | INF | INVALID

let classify (x:t) : kind =
  if classify_float x = FP_nan then INVALID
  else if x = infinity then INF
  else if x = neg_infinity then MINF
  else FINITE



(* useful constants *)

let zero : t = 0.
let one : t = 1.
let two : t = 2.
let minus_one : t = -1.
let inf : t = infinity
let minus_inf : t = neg_infinity
let nan : t = nan


(* exact operators *)

let neg x = -. x
let abs x = abs_float x


(* operators with rounding *)
let add_up a b = a +. b
let sub_up a b = a -. b
let mul_up a b = a *. b
let div_up a b =
  match sign a, sign b with
  |  0,_ -> zero
  |  1,0 -> inf
  | -1,0 -> minus_inf
  | _ -> a /. b

let add_down a b = -. (-. a -. b)
let sub_down a b = -. (b -. a)
let mul_down a b = -. ((-. a) *. b)
let div_down a b =
    match sign a, sign b with
    |  0,_ -> zero
    |  1,0 -> inf
    | -1,0 -> minus_inf
    | _ -> -. ((-. a) /. b)

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
let sqrt_up x = sqrt x
let sqrt_down x = div_down 1. (sqrt (div_up 1. x))

let pow_up x n = x ** (float n)
let pow_down x n = div_down 1. ((div_up 1. x) ** (float n))

let root_up x n =
  if x<0. && n mod 2 = 1 then -. (exp((log (-.x)) /. (float n)))
  else exp((log x) /. (float n))

let root_down x n =
  if x<0. && n mod 2 = 1 then -. (exp((log (-.x)) /. (float n)))
  else exp((log x) /. (float n))

let pi = 3.141592653589793
let twopi = 2.*.pi

(* returns true is cos is monotonic and strictly decreasing around x; x in [0;2pi] *)
let is_cos_decreasing x = x > 0. && x < pi

let to_zero_twopi x = x -. ((x /. twopi) |> floor) *. twopi

(* returns true is cos is monotonic and strictly decreasing around x*)
let is_cos_decreasing x = is_cos_decreasing (to_zero_twopi x)

let is_sin_decreasing x = is_cos_decreasing (x +. pi/.2.)

(* let cos_up x = if is_cos_decreasing x then cos x else -.(cos (-.x)) *)

(* let cos_down x =  *)
(*   if is_cos_decreasing x then -. (cos (-.x)) else cos x *)

(* let sin_up x =  *)
(*   if is_sin_decreasing x then sin x else -.(sin (-.x)) *)

(* let sin_down x =  *)
(*   if is_sin_decreasing x then -.(sin (-.x)) else sin x *)

let cos_up = cos
let cos_down = cos

let sin_up = sin
let sin_down = sin

let tan_up = tan
let tan_down = tan

let acos_up = acos
let acos_down x = acos x

let asin_up = asin
let asin_down x = asin x

let atan_up = atan
let atan_down = atan

let exp_up = exp
let exp_down x = div_down 1. (exp_up (-. x))

let ln_up = log
let ln_down = log

let log_up = log10
let log_down = log10


(* double-precision characteristics *)

let mant_size = 52
let min_exp = -1022
let max_exp = 1023
let min_denormal = ldexp 1. (min_exp-mant_size)
let min_normal = ldexp 1. min_exp
let max_normal = ldexp (2. -. ldexp 1. (-mant_size)) max_exp
let max_exact = ldexp 1. mant_size
let ulp = ldexp 1. (-mant_size)

let floor = floor
let ceil = ceil
