(** Module type arithmetic types equipped with addition and multiplication. *)
module type T = sig
  type t

  val add : t -> t -> t

  val mul : t -> t -> t

  val div : t -> t -> t option
  (** * [div p1 p2] returns the division of p1 by p2. * None if the division is
      not exact *)

  val neg : t -> t

  val to_int : t -> int option
  (** None if the value cannot be converted exactly to an integer *)

  val to_float : t -> float

  val to_rational : t -> Mpqf.t

  val floor : t -> int

  val zero : t
  (** constants *)

  val one : t

  val of_int : int -> t
  (** constructors *)

  val of_float : float -> t

  val of_rational : Mpqf.t -> t

  val equal : t -> t -> bool
  (** equality *)

  val compare : t -> t -> int
  (** Total order *)

  val print : Format.formatter -> t -> unit
  (** printintg *)

  val to_string : t -> string
end
