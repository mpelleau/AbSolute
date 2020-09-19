(** Float rings *)
type t = float

let add = ( +. )
let mul = ( *. )
let zero = 0.
let one = 1.

let div x y = if y <> 0. && (x/.y) *. y = x then Some (x/.y) else None

let neg x = -1. *. x

let to_int x =
  let xi = int_of_float x in
  if float xi = x then Some xi
  else None
let to_float = Fun.id
let to_rational = Mpqf.of_float
let floor = int_of_float

let of_int = float_of_int
let of_float = Fun.id
let of_rational = Mpqf.to_float

let equal : t -> t -> bool = (=)
let compare = Float.compare

let print = Tools.pp_print_float
let to_string = string_of_float
