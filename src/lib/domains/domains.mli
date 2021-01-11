(** Type of generic functors that lift a numeric domain to a boolean one *)
module type B = sig
  module Make : functor (N : Signature.Numeric) -> Signature.Domain
end

(** Type of domain combinator of arity 1 (e.g powerset) *)
module type D1 = sig
  module Make : functor (D : Signature.Domain) -> Signature.Domain
end

(** Type of domain combinator of arity 2 (e.g products) *)
module type D2 = sig
  module Make : functor (A : Signature.Domain) (B : Signature.Domain) ->
    Signature.Domain
end

val get_all : unit -> string list
(** collect the list of all registered domain names *)

val register_numeric : string -> (module Signature.Numeric) -> unit
(** registers a numeric domain into the list of available abstract domains *)

val register_boolean : string -> (module B) -> unit
(** registers a boolean domain into the list of available abstract domains *)

val register1 : string -> (module D1) -> unit
(** registers a domain combinator into the list of combinators of arity 1 *)

val register2 : string -> (module D2) -> unit
(** registers a domain combinator into the list of combinators of arity 2 *)

val parse : string -> string -> (module Signature.Domain)
(** builds the abstract domain corresponding to the name of the numeric
    representaion and boolean representation given in parameter *)
