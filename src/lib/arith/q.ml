type t   = Mpqf.t

(** {1 Constructors} *)

let zero = Mpqf.of_int 0
let one  = Mpqf.of_int 1
let two  = Mpqf.of_int 2
let minus_one = Mpqf.of_int (-1)

(** {1 Operators} *)

let add  = Mpqf.add

let mul  = Mpqf.mul

let div x y = if not (Mpqf.equal zero y) then Some (Mpqf.div x y) else None

let neg x = Mpqf.neg x

let ceil x = x |> Mpqf.to_float |> ceil |> int_of_float
let floor x = x |> Mpqf.to_float |> floor |> int_of_float

(** {1 Conversions} *)
let to_int x =
  let xi = int_of_float (Mpqf.to_float x) in
  if Mpqf.equal x (Mpqf.of_int xi) then Some xi else None

let to_float = Mpqf.to_float

let to_rational : t -> t = Fun.id

let of_int = Mpqf.of_int
let of_float = Mpqf.of_float
let of_rational = Fun.id

(** {1 Comparisons} *)

let equal = Mpqf.equal
let compare = Mpqf.cmp

(** {1 Printing} *)

let print fmt x = Format.fprintf fmt "%s" (Mpqf.to_string x)
let to_string = Mpqf.to_string
