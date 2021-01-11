(** This module handle the domain environment, already equipped with some basic
    domains. You can add your own using the [register_*] functions, or the
    already defined ones. *)

open Signature

(** Type of generic functors that lift a numeric domain to a boolean one *)
module type B = sig
  module Make : functor (N : Numeric) -> Domain
end

(** Type of domain combinator of arity 1 (e.g powerset) *)
module type D1 = sig
  module Make : functor (D : Domain) -> Domain
end

(** Type of domain combinator of arity 2 (e.g products) *)
module type D2 = sig
  module Make : functor (A : Domain) (B : Domain) -> Domain
end

val get_all : unit -> string list
(** collect the list of all registered domain names *)

val register_numeric : string -> (module Numeric) -> unit
(** registers a numeric domain into the list of available abstract domains *)

val register_boolean : string -> (module B) -> unit
(** registers a boolean domain into the list of available abstract domains *)

val register_1 : string -> (module D1) -> unit
(** registers a domain combinator into the list of combinators of arity 1 *)

val register_2 : string -> (module D2) -> unit
(** registers a domain combinator into the list of combinators of arity 2 *)

val parse : string -> string -> (module Domain)
(** builds the abstract domain corresponding to the name of the numeric
    representaion and boolean representation given in parameter *)
