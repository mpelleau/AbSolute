(*
  An abstract fixpoint solver based on Constraint Programming

  Author: Antoine Mine
  Copyright 2015
*)

(* Signature for bounds: numeric values enriched with +oo and -oo. *)

module type BOUND = sig

  type t

  (* ordering *)
  (* ******** *)

  val compare: t -> t -> int

  val equal: t -> t -> bool
  val leq: t -> t -> bool
  val geq: t -> t -> bool
  val lt: t -> t -> bool
  val gt: t -> t -> bool
  val neq: t -> t -> bool

  val odd: t -> bool
  val even: t -> bool

  val min: t -> t -> t
  val max: t -> t -> t

  val sign: t -> int

  (** `succ` and `prec` returns the number right after the current one, if it exists.
      Otherwise, it acts as the identity function.
      For example, it exists for integers, but not for float and rational. *)
  val succ: t -> t
  val prec: t -> t

  (* construction *)
  (* ************ *)

  (* operators and conversions are tagged with a _up or _down suffix
     to indicate rounding direction
   *)

  val of_int_up: int -> t
  val of_int_down: int -> t
  (* val of_z_up: Z.t -> t *)
  (* val of_z_down: Z.t -> t *)
  val of_float_up: float -> t
  val of_float_down: float -> t
  val of_rat_up: Bound_rat.t -> t
  val of_rat_down: Bound_rat.t -> t

  val of_string_up: string -> t
  val of_string_down: string -> t

  (* printing *)
  (* ******** *)

  val to_string: t -> string
  val output: out_channel -> t -> unit
  val sprint: unit -> t -> string
  val bprint: Buffer.t -> t -> unit
  val pp_print: Format.formatter -> t -> unit

  (* conversion *)
  (* ********** *)

  val to_float_up: t -> float
  val to_float_down: t -> float
  val to_rat: t -> Bound_rat.t
  val to_int_up: t -> int
  val to_int_down: t -> int

  (* classification *)
  (* ************** *)

  type kind =
    | FINITE      (* finite number *)
    | MINF | INF  (* -oo or +oo *)
    | INVALID     (* for NaN and other invalid numbers *)

  val classify: t -> kind

  (* useful constants *)
  (* **************** *)

  val zero: t
  val one: t
  val two: t
  val minus_one: t
  val inf: t
  val minus_inf: t


  (* operators *)
  (* ********* *)

  (* exact operators *)
  val neg: t -> t
  val abs: t -> t

  (* operators with rounding *)
  val add_up: t -> t -> t
  val sub_up: t -> t -> t
  val mul_up: t -> t -> t
  val div_up: t -> t -> t
  val add_down: t -> t -> t
  val sub_down: t -> t -> t
  val mul_down: t -> t -> t
  val div_down: t -> t -> t

  val bound_mul: (t -> t -> t) -> t -> t -> t
  val bound_div: (t -> t -> t) -> t -> t -> t

  val sqrt_up: t -> t
  val sqrt_down: t -> t

  val pow_up: t -> int -> t
  val pow_down: t -> int -> t
  val root_up: t -> int -> t
  val root_down: t -> int -> t

  val cos_up: t -> t
  val cos_down: t -> t
  val sin_up: t -> t
  val sin_down: t -> t
  val tan_up: t -> t
  val tan_down: t -> t

  val acos_up: t -> t
  val acos_down: t -> t
  val asin_up: t -> t
  val asin_down: t -> t
  val atan_up: t -> t
  val atan_down: t -> t

  val exp_up: t -> t
  val exp_down: t -> t
  val ln_up: t -> t
  val ln_down: t -> t
  val log_up: t -> t
  val log_down: t -> t

  (* integer rounding *)
  val floor: t -> t
  val ceil: t -> t

end
