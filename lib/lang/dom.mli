(** This module defines the different kinds of definition domains of variables *)

(** main type *)
type t =
  | Finite of Q.t * Q.t  (** \[a;b\] *)
  | Minf of Q.t  (** \]-oo; a\] *)
  | Inf of Q.t  (** \[a; +oo\[ *)
  | Set of Q.t list  (** \{x1; x2; ...; xn\} *)
  | Top  (** \]-oo; +oo\[ *)

(** {1 Constructors} *)

val interval : Q.t -> Q.t -> t
(** \[a;b\]*)

val of_ints : int -> int -> t

val of_floats : float -> float -> t

val inf : Q.t -> t
(** semi-open interval : \[x;+oo\[*)

val set : Q.t list -> t
(** finite sets of values \{x1; x2; ...; xn\} *)

val minf : Q.t -> t
(** semi-open interval : \]-oo;x\]*)

val top : t
(** \]-oo;+oo\[*)

(** {1 Predicates} *)

val belong : Q.t -> t -> bool
(** checks if a given rational belongs to a domain *)

val is_bounded : t -> bool

(** {1 Operations} *)

(** Given a variable name [v] and a domain [d], builds the weakest constraint
    that [v] should respect to be in [d]. *)

val to_constraint : string -> t -> Constraint.t
(** Given a variable name [v] and a domain [d], builds the weakest constraint
    that [v] should respect to be in [d]. *)

(** {1 Printing} *)

val print : Format.formatter -> t -> unit
(** printer *)

val to_string : t -> string
(** Conversion to a string *)
