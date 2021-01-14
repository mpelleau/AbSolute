(** Integer rings *)

type t = int

let add = Int.add

let mul = Int.mul

let zero = 0

let one = 1

let div x y = if y <> 0 && x mod y = 0 then Some (x / y) else None

let neg = Int.neg

let to_int x = Some x

let to_float = float_of_int

let to_rational = Mpqf.of_int

let floor = Fun.id

let of_int = Fun.id

let of_float = int_of_float

let of_rational x = of_float (Mpqf.to_float x)

let equal : t -> t -> bool = ( = )

let compare = Int.compare

let print fmt x = Format.fprintf fmt "%i" x

let to_string = string_of_int
