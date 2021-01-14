type t = float

(** {1 Constructors} *)

let zero = 0.

let one = 1.

let two = 2.

let minus_one = -1.

(** {1 Operators} *)

let add = Float.add

let mul = Float.mul

let div x y = if y <> 0. && x /. y *. y = x then Some (x /. y) else None

let neg x = -1. *. x

let ceil x = ceil x |> int_of_float

let floor x = floor x |> int_of_float

(** {1 Conversions} *)

let to_int x =
  let xi = int_of_float x in
  if float xi = x then Some xi else None

let to_float = Fun.id

let to_rational = Mpqf.of_float

let of_int = float_of_int

let of_float = Fun.id

let of_rational = Mpqf.to_float

(** {1 Comparisons} *)

let equal : t -> t -> bool = ( = )

let compare = Float.compare

(** {1 Printing} *)

(** float light printing when decimal part is nul *)
let pp_print fmt (f : float) =
  let i = int_of_float f in
  if float i = f then Format.fprintf fmt "%i" i else Format.pp_print_float fmt f

let print = pp_print

let to_string = string_of_float
