(** Functor parametrized by an abstract domain and defines the three main
    solving functions *)

module Make : functor (D : Signature.Domain) -> sig
  val coverage : ?verbose:bool -> float -> int -> Csp.t -> D.t Result.t
  (** coverage of the solution space*)

  val satisfiability : ?verbose:bool -> float -> int -> Csp.t -> Kleene.t
  (** satisfiability check *)

  val witness :
    ?verbose:bool -> float -> int -> Csp.t -> Kleene.t * Csp.instance option
  (** satisfiability check, with witness *)
end
