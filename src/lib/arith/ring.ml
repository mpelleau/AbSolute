(** Module type arithmetic types equipped with addition and
   multiplication. *)
module type T = sig
  type t

  val add   : t -> t -> t
  val mul   : t -> t -> t

  (**
   * [div p1 p2] returns the division of p1 by p2.
   * None if the division is not exact *)
  val div   : t -> t -> t option

  val neg : t -> t

  (** None if the value cannot be converted exactly to an integer *)
  val to_int : t -> int option

  val to_float : t -> float
  val to_rational : t -> Mpqf.t
  val floor : t -> int

  (** constants *)
  val zero  : t
  val one   : t

  (** constructors *)
  val of_int : int -> t
  val of_float : float -> t
  val of_rational : Mpqf.t -> t

  (** equality *)
  val equal : t -> t -> bool

  (** Total order *)
  val compare : t -> t -> int

  (** printintg *)
  val print : Format.formatter -> t -> unit
  val to_string : t -> string
end
