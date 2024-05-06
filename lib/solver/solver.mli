(** Functor parametrized by an abstract domain and defines the three main
    solving functions *)

module Make : functor (D : Signature.Domain) (P : Signature.Propagator) -> sig
  val coverage : ?verbose:bool -> float -> int -> Csp.t -> D.t Result.t
  (** coverage of the solution space*)

  val satisfiability : ?verbose:bool -> float -> int -> Csp.t -> Kleene.t
  (** satisfiability check *)

  val witness : ?verbose:bool -> float -> int -> Csp.t -> Consistency.feasible
  (** satisfiability check, with witness *)
end

(** Default configuration of the solver *)
module Default : sig
  val coverage :
       ?verbose:bool
    -> float
    -> int
    -> Csp.t
    -> Domains.Boolean.Make(Domains.BoxS).t Result.t

  val satisfiability : ?verbose:bool -> float -> int -> Csp.t -> Kleene.t

  val witness : ?verbose:bool -> float -> int -> Csp.t -> Consistency.feasible
end
