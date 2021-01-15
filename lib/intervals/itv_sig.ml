(** Generic signature for intervals. The interface is functional.*)

module type ITV_EVAL = sig
  type t

  val top_int : t

  val top_real : t

  val of_ints : int -> int -> t
  (** constructors *)

  val of_rats : Q.t -> Q.t -> t

  val of_floats : float -> float -> t

  val of_int : int -> t
  (** singletons *)

  val of_rat : Q.t -> t

  val of_float : float -> t

  val to_float_range : t -> float * float
  (** {1 PRINTING and CONVERSIONS} *)

  val to_rational_range : t -> Q.t * Q.t

  val to_annot : t -> Csp.typ
  (** returns the type annotation of the represented values *)

  val print : Format.formatter -> t -> unit

  val to_bexpr : string -> t -> Constraint.t
  (** converts the interval to a boolean constraint *)

  val float_size : t -> float
  (** measure *)

  val score : t -> float

  (** {1 SET-THEORETIC} *)

  val join : t -> t -> t
  (** operations *)

  val meet : t -> t -> t option

  val split : t -> t list

  val prune : (t -> t -> t list) option

  val contains_float : t -> float -> bool
  (** predicates *)

  val is_positive : t -> bool

  val is_negative : t -> bool

  val is_singleton : t -> bool

  val neg : t -> t
  (** {1 INTERVAL ARITHMETICS (FORWARD EVALUATION)} *)

  val abs : t -> t

  val add : t -> t -> t

  val sub : t -> t -> t

  val mul : t -> t -> t

  val div : t -> t -> t option
  (** return valid values (possibly Bot, if dividend is nul) *)

  val min : t -> t -> t
  (** min and max over intervals *)

  val max : t -> t -> t

  val pow : t -> t -> t
  (** returns valid value when the exponant is a singleton positive integer.
      fails otherwise*)

  val n_root : t -> t -> t option

  val exp : t -> t

  val ln : t -> t option

  val log : t -> t option

  val eval_fun : string -> t list -> t option
  (** function calls (sqrt, exp, ln ...) are handled here : given a function
      name and and a list of argument, it returns a possibly bottom result *)

  val spawn : t -> float
  (** generate a random float within the given interval *)
end

module type ITV = sig
  (** {1 FILTERING (TEST TRANSFER FUNCTIONS)} *)
  include ITV_EVAL

  val filter_leq : t -> t -> (t * t) Consistency.t
  (** given two interval arguments, return a filtered subset of each argument by
      removing points that cannot satisfy the predicate; may also return Sat or
      Unsat if all (resp. no) point can satisfy the predicate. Interface is
      simplified since a > b <=> b < a *)

  val filter_lt : t -> t -> (t * t) Consistency.t

  val filter_neq : t -> t -> (t * t) Consistency.t

  val filter_eq : t -> t -> t Consistency.t

  val filter_neg : t -> t -> t option
  (** given the interval argument(s) and the expected interval result of a
      numeric operation, returns refined interval argument(s) where points that
      cannot contribute to a value in the result are removed; may also return
      Bot if no point in an argument can lead to a point in the result *)

  val filter_abs : t -> t -> t option

  val filter_add : t -> t -> t -> (t * t) option

  val filter_sub : t -> t -> t -> (t * t) option

  val filter_mul : t -> t -> t -> (t * t) option

  val filter_div : t -> t -> t -> (t * t) option

  val filter_pow : t -> t -> t -> (t * t) option

  val filter_fun : string -> t list -> t -> t list option
  (** filtering function calls like (sqrt, exp, ln ...) is done here : given a
      function name, a list of argument, and a result, it remove points that
      cannot satisfy the relation : f(arg1,..,argn) = r; it returns a possibly
      bottom result *)
end
