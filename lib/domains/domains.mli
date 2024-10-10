(** This module handle the domain environment, already equipped with some basic
    domains. You can add your own using the [register_*] functions, and combine
    them with the already defined ones. *)

open Signature

(** Type of generic functors that lift a numeric domain to a boolean one *)
module type B = sig
  module Make : functor (N : Numeric) -> Domain
end

(** Type of domain combinator of arity 1 (e.g powerset) *)
module type D1 = sig
  module Make : functor (D : Domain) -> Domain
end

(** Products combinator *)
module type D2 = sig
  module Make : functor (R : Reduction) -> Domain
end

(** {1 Domain environment management} *)

(* val get_numeric : unit -> string list
 * (\** collect the list of registered numeric domains *\)
 *
 * val get_boolean : unit -> string list
 * (\** collect the list of registered boolean domains *\) *)

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
    representaion and boolean representation given in parameter. Domain
    application follows the syntax : ["combinator(domain1,domain2)"] where
    [combinator] has to be registered with the [register_2] function (or be one
    of the predefined comibnator), and both [domain1] and [domain2] has to be
    registered. Useful to build a domain from a command line description *)

val iterator : unit -> (module Propagator)
(** current propagation scheme *)

(** {1 Predefined Abstract Domains} *)

(** {2 Numeric domains} *)

(** Boxes with floatting point included bounds *)
module BoxF : Numeric

(** Boxes with floatting point included or excluded bounds *)
module BoxS : Numeric

(** Apron Boxes *)
module ApronBox : Numeric

(** Apron Polyhedra *)
module Poly : Numeric

(** Apron Octagons *)
module Oct : Numeric

(** Equalities *)
module Alias : Numeric

(** {2 Boolean domains} *)

(** Lfts a numerical domain to a boolean one *)
module Boolean : B

(** Disjunctive form with fast-precomputation for meets *)
module Utree : B

(** {2 Combinators} *)

(** Specialized Reduced Product. Corresponds to the option [-d product (a,b)] of
    AbSolute. if a constraint can be filtered exactly by the domain [b], it is
    affected to it, otherwise it is affected to [b].*)
module Product : D2
