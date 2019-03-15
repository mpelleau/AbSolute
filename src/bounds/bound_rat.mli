type t
val name : string
val is_continuous: bool
val succ: t -> t
val prec: t -> t
val zero : t
val one : t
val two : t
val minus_one : t
val minus_two : t
val inf : t
val minus_inf : t
val rat_nan : t
type kind = FINITE | MINF | INF | INVALID
val classify : t -> kind
val compare' : t -> t -> int -> int
val compare : t -> t -> int
val equal : t -> t -> bool
val leq : t -> t -> bool
val geq : t -> t -> bool
val lt : t -> t -> bool
val gt : t -> t -> bool
val neq : t -> t -> bool
val min : t -> t -> t
val max : t -> t -> t
val sign : t -> int
val odd : t -> bool
val even : t -> bool
val int_inf : int
val int_minus_inf : int
val of_int : int -> t
val of_int_up : int -> t
val of_int_down : int -> t
val of_float : float -> t
val of_float_up : float -> t
val of_float_down : float -> t
val of_rat_up : t -> t
val of_rat_down : t -> t
val of_string : string -> t
val of_string_up : string -> t
val of_string_down : string -> t
val of_frac : int -> int -> t
val wrap : (t -> 'a) -> 'a -> 'a -> 'a -> t -> 'a
val to_string : t -> string
val to_mpqf: t -> Mpqf.t
val of_mpqf: Mpqf.t -> t
val to_float_up : t -> float
val to_float_down : t -> float
val to_int_up: t -> int
val to_int_down: t -> int
val to_rat : 'a -> 'a
val output : out_channel -> t -> unit
val sprint : unit -> t -> string
val bprint : Buffer.t -> t -> unit
val pp_print : Format.formatter -> t -> unit
val neg : t -> t
val abs : t -> t
val wrap2 : (t -> t -> t) -> (float -> float -> float) -> t -> t -> t
val add : t -> t -> t
val sub : t -> t -> t
val mul : t -> t -> t
val div : t -> t -> t
val add_up : t -> t -> t
val sub_up : t -> t -> t
val mul_up : t -> t -> t
val div_up : t -> t -> t
val add_down : t -> t -> t
val sub_down : t -> t -> t
val mul_down : t -> t -> t
val div_down : t -> t -> t
val bound_mul : (t -> t -> t) -> t -> t -> t
val bound_div : (t -> t -> t) -> t -> t -> t
val sqrt_up : t -> t
val sqrt_down : t -> t
val cos_up : t -> t
val cos_down : t -> t
val sin_up : t -> t
val sin_down : t -> t
val tan_up : t -> t
val tan_down : t -> t
val acos_up : t -> t
val acos_down : t -> t
val asin_up : t -> t
val asin_down : t -> t
val atan_up : t -> t
val atan_down : t -> t
val exp_up : t -> t
val exp_down : t -> t
val ln_up : t -> t
val ln_down : t -> t
val log_up : t -> t
val log_down : t -> t
val floor : t -> t
val ceil : t -> t
val pow_up : t -> int -> t
val pow_down : t -> int -> t
val root_up : t -> int -> t
val root_down : t -> int -> t
