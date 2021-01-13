type t = Mpqf.t

(* ordering *)

let name = "rational"

let compare (a : t) (b : t) = Mpqf.cmp a b

let equal (a : t) (b : t) = Mpqf.equal a b

let leq (x : t) (y : t) : bool = Mpqf.cmp x y <= 0

let geq (x : t) (y : t) : bool = Mpqf.cmp x y >= 0

let lt (x : t) (y : t) : bool = Mpqf.cmp x y < 0

let gt (x : t) (y : t) : bool = Mpqf.cmp x y > 0

let neq (x : t) (y : t) : bool = not (Mpqf.equal x y)

let min (x : t) (y : t) : t = if Mpqf.cmp x y <= 0 then x else y

let max (x : t) (y : t) : t = if Mpqf.cmp x y >= 0 then x else y

let sign (x : t) : int = Mpqf.sgn x

let odd (x : t) : bool =
  int_of_float (Mpqf.to_float x) / 2 * 2 |> float <> Mpqf.to_float x

let even (x : t) : bool =
  int_of_float (Mpqf.to_float x) / 2 * 2 |> float = Mpqf.to_float x

(* conversion, printing *)

let of_int_up a = Mpqf.of_int a

let of_int_down a = Mpqf.of_int a

let of_float_up a : t = Mpqf.of_float a

let of_float_down a : t = Mpqf.of_float a

let of_rat_up a : t = a

let of_rat_down a : t = a

let of_string x = Mpqf.of_string x

(* TODO *)
let of_string_up = of_string

let of_string_down = of_string

(* Note: adds 0. to favor positive 0 *)
let to_string x = (*string_of_float (Mpqf.to_float x)*) Mpqf.to_string x

let to_float_up x : float = Mpqf.to_float x

let to_float_down x : float = -.Mpqf.to_float (Mpqf.neg x)

let to_rat x = x

(* printing *)
let output chan x = output_string chan (to_string x)

let sprint () x = to_string x

let bprint b x = Buffer.add_string b (to_string x)

let pp_print f x = Format.pp_print_string f (to_string x)

(* classification *)

type kind = FINITE | MINF | INF | INVALID

let classify (x : t) : kind =
  let num, den = Mpqf.to_mpzf2 x in
  match (Mpzf.sgn num, Mpzf.sgn den) with
  | 0, 0 -> INVALID
  | 1, 0 -> INF
  | -1, 0 -> MINF
  | _ -> FINITE

(* useful constants *)

let zero : t = Q.zero

let one : t = Q.one

let two : t = Q.two

let minus_one : t = Q.minus_one

let inf : t = Mpqf.of_frac 1 0

let minus_inf : t = Mpqf.of_frac (-1) 0

let nan : t = Mpqf.of_frac 0 0

(* exact operators *)

let neg x = Mpqf.neg x

let abs x = Mpqf.abs x

(* operators with rounding *)

let add_up a b = Mpqf.add a b

let sub_up a b = Mpqf.sub a b

let mul_up a b = Mpqf.mul a b

let div_up a b = Mpqf.div a b

let add_down a b = Mpqf.add a b

let sub_down a b = Mpqf.sub a b

let mul_down a b = Mpqf.mul a b

let div_down a b = Mpqf.div a b

(* helper: oo * 0 = 0 when multiplying bounds *)
let bound_mul f x y = if sign x = 0 || sign y = 0 then zero else f x y

(* helper: 0/0 = 0, x/0 = sign(x) oo *)
let bound_div f x y =
  match (sign x, sign y) with
  | 0, _ -> zero
  | 1, 0 -> inf
  | -1, 0 -> minus_inf
  | _ -> f x y

(* TODO: improve and check soundness *)
let sqrt_up x = Mpqf.of_float (sqrt (Mpqf.to_float x))

let sqrt_down x = Mpqf.of_float (-.(-1. /. sqrt (1. /. Mpqf.to_float x)))

let cos_up x = Mpqf.of_float (cos (Mpqf.to_float x))

let cos_down x = Mpqf.of_float (cos (Mpqf.to_float x))

let sin_up x = Mpqf.of_float (sin (Mpqf.to_float x))

let sin_down x = Mpqf.of_float (sin (Mpqf.to_float x))

let tan_up x = Mpqf.of_float (tan (Mpqf.to_float x))

let tan_down x = Mpqf.of_float (tan (Mpqf.to_float x))

let acos_up x = Mpqf.of_float (acos (Mpqf.to_float x))

let acos_down x = Mpqf.of_float (acos (Mpqf.to_float x))

let asin_up x = Mpqf.of_float (asin (Mpqf.to_float x))

let asin_down x = Mpqf.of_float (asin (Mpqf.to_float x))

let atan_up x = Mpqf.of_float (atan (Mpqf.to_float x))

let atan_down x = Mpqf.of_float (atan (Mpqf.to_float x))

let exp_up x = Mpqf.of_float (exp (Mpqf.to_float x))

let exp_down x = Mpqf.of_float (exp (Mpqf.to_float x))

let ln_up x = Mpqf.of_float (log (Mpqf.to_float x))

let ln_down x = Mpqf.of_float (log (Mpqf.to_float x))

let log_up x = Mpqf.of_float (log10 (Mpqf.to_float x))

let log_down x = Mpqf.of_float (log10 (Mpqf.to_float x))

let floor x = Mpqf.of_float (floor (Mpqf.to_float x))

let ceil x = Mpqf.of_float (ceil (Mpqf.to_float x))

let pow_up x n = Mpqf.of_float (Mpqf.to_float x ** float n)

let pow_down x n = Mpqf.of_float (-.(-.Mpqf.to_float x ** float n))

let root_up x n = Mpqf.of_float (exp (log (Mpqf.to_float x) /. float n))

let root_down x n = Mpqf.of_float (exp (log (Mpqf.to_float x) /. float n))
