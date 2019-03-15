(* We encode -infinity and infinity as `min_int` and `max_int`. *)
type t = int

(* ordering *)

let name = "integer"

exception NotAnIntegerNumber

let compare (a:t) (b:t) = compare a b
let equal (a:t) (b:t) = a = b
let leq (x:t) (y:t) : bool = x <= y
let geq (x:t) (y:t) : bool = x >= y
let lt (x:t) (y:t) : bool = x < y
let gt (x:t) (y:t) : bool = x > y
let neq (x:t) (y:t) : bool = x <> y

let odd (x:t) : bool = x mod 2 = 1
let even (x:t) : bool = x mod 2 = 0

let min (x:t) (y:t) : t = min x y
let max (x:t) (y:t) : t = max x y

let sign (x:t) : int =
  if x > 0 then 1 else
  if x < 0 then -1 else 0

(* useful constants *)

let zero : t = 0
let one : t = 1
let two : t = 2
let minus_one : t = -1
let inf : t = max_int
let minus_inf : t = min_int

(* conversion, printing *)

(* We convert infinite floating point values to our infinity integer encoding. *)
let fwrap (a:float) op =
  if a = infinity then inf
  else if a = neg_infinity then minus_inf
  else if a = nan then raise NotAnIntegerNumber
  else op a

let of_int_up a = a
let of_int_down a = a
let of_float_up a : t = fwrap a (fun a -> int_of_float (ceil a))
let of_float_down a : t = fwrap a (fun a -> int_of_float (floor a))

let of_rat_down a : t =
  match Bound_rat.classify a with
  | Bound_rat.FINITE -> fwrap (Bound_rat.to_float_down a) of_float_down
  | Bound_rat.INF -> inf
  | Bound_rat.MINF -> minus_inf
  | Bound_rat.INVALID -> raise NotAnIntegerNumber

let of_rat_up a : t =
  match Bound_rat.classify a with
  | Bound_rat.FINITE -> fwrap (Bound_rat.to_float_up a) of_float_up
  | Bound_rat.INF -> inf
  | Bound_rat.MINF -> minus_inf
  | Bound_rat.INVALID -> raise NotAnIntegerNumber

let of_string = int_of_string

let of_string_up = of_string
let of_string_down = of_string

let to_string a =
  if a = inf then "inf"
  else if a = minus_inf then "-inf"
  else string_of_int a

let iwrap (a:t) op =
  if a = inf then infinity
  else if a = minus_inf then neg_infinity
  else op a

let to_float_up x : float = iwrap x Bound_float.of_int_up
let to_float_down x : float = iwrap x Bound_float.of_int_down
let to_rat x = Bound_rat.of_int x
let to_int_up x = x
let to_int_down x = x

(* printing *)
let output chan x = output_string chan (to_string x)
let sprint () x = to_string x
let bprint b x = Buffer.add_string b (to_string x)
let pp_print f x = Format.pp_print_string f (to_string x)

(* classification *)

type kind = FINITE | MINF | INF | INVALID

let classify (x:t) : kind =
  if x = inf then INF
  else if x = minus_inf then MINF
  else FINITE

(* exact operators *)
let is_continuous = false
let succ x = if x = inf || x = minus_inf then x else x + 1
let prec x = if x = inf || x = minus_inf then x else x - 1

let neg x =
  if x = inf then minus_inf
  else if x = minus_inf then inf
  else -x

let abs x = if x = minus_inf then inf else abs x

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

(* We first compute the result with floating point to see if it overflows the integer representation, in which case the result is infinity. *)
let wrap fop iop a b =
  let fres = fop (to_float_up a) (to_float_up b) in
  if fres = nan then raise NotAnIntegerNumber
  else if fres <= (float_of_int minus_inf) then minus_inf
  else if fres >= (float_of_int inf) then inf
  else iop a b

let add_up a b = wrap (+.) (+) a b
let sub_up a b = wrap (-.) (-) a b
let mul_up a b = wrap ( *.) ( * ) a b
let div_up a b = wrap (/.) (fun a b -> (a / b) + (b mod 2)) a b

let add_down = add_up
let sub_down = sub_up
let mul_down = mul_up
let div_down a b = wrap (/.) (/) a b

let fwrap_up fop x = fwrap (fop (to_float_up x)) of_float_up
let fwrap_down fop x = fwrap (fop (to_float_down x)) of_float_down

let sqrt_up x = fwrap_up Bound_float.sqrt_up x
let sqrt_down x = fwrap_down Bound_float.sqrt_down x

let pow_up x n = fwrap_up (fun x -> Bound_float.pow_up x n) x
let pow_down x n = fwrap_down (fun x -> Bound_float.pow_down x n) x

let root_up x n = fwrap_up (fun x -> Bound_float.root_up x n) x
let root_down x n = fwrap_down (fun x -> Bound_float.root_down x n) x

let cos_up x = fwrap_up Bound_float.cos_up x
let cos_down x = fwrap_down Bound_float.cos_down x

let sin_up x = fwrap_up Bound_float.sin_up x
let sin_down x = fwrap_down Bound_float.sin_down x

let tan_up x = fwrap_up Bound_float.tan_up x
let tan_down x = fwrap_down Bound_float.tan_down x

let acos_up x = fwrap_up Bound_float.acos_up x
let acos_down x = fwrap_down Bound_float.acos_down x

let asin_up x = fwrap_up Bound_float.asin_up x
let asin_down x = fwrap_down Bound_float.asin_down x

let atan_up x = fwrap_up Bound_float.atan_up x
let atan_down x = fwrap_down Bound_float.atan_down x

let exp_up x = fwrap_up Bound_float.exp_up x
let exp_down x = fwrap_down Bound_float.exp_down x

let ln_up x = fwrap_up Bound_float.ln_up x
let ln_down x = fwrap_down Bound_float.ln_down x

let log_up x = fwrap_up Bound_float.log_up x
let log_down x = fwrap_down Bound_float.log_down x

let floor x = x
let ceil x = x
