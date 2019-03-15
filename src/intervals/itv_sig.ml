(** Generic signature for intervals. The interface is functional.*)

open Bot

module type ITV = sig

  (************************************************************************)
  (** {1 TYPES} *)
  (************************************************************************)

  (** an interval is a pair of bounds (lower,upper);
     intervals are always non-empty: lower <= upper;
     functions that can return an empty interval return it as Bot *)
  type t
  module B: Bound_sig.BOUND
  type bound = B.t

  (************************************************************************)
  (** {1 CONSTRUCTORS AND CONSTANTS} *)
  (************************************************************************)

  (** default value for unconstrained variables *)
  val top_int: t
  val top_real: t
  val top: t

  val of_bounds : bound -> bound -> t
  val of_ints: int -> int -> t
  val of_rats: Bound_rat.t -> Bound_rat.t -> t
  val of_floats: float -> float -> t

  val of_int: int -> t
  val of_rat: Bound_rat.t -> t
  val of_float: float -> t

  val zero: t
  val one: t

  (************************************************************************)
  (** {1 PRINTING and CONVERSIONS } *)
  (************************************************************************)

  val to_float_range : t -> float * float
  val to_rational_range : t -> Bound_rat.t * Bound_rat.t
  val to_range : t -> bound * bound
  val lb: t -> bound
  val ub: t -> bound

  (** returns the type annotation of the represented values *)
  val to_annot : t -> Csp.annot
  val print: Format.formatter -> t -> unit

  val to_expr: t -> (Csp.cmpop * Csp.expr) * (Csp.cmpop * Csp.expr)

  (************************************************************************)
  (** {1 SET-THEORETIC } *)
  (************************************************************************)

  (** operations *)

  val join: t -> t -> t
  val meet: t -> t -> t bot

  (** predicates *)
  val contains_float: t -> float -> bool

  val is_singleton: t -> bool
  val check_bot: t -> t bot

  val equal : t -> t -> bool

  (** mesure *)
  val float_size: t -> float

  (** split *)

  (** returns a split priority. The higher the better *)
  val score : t -> float

  (** Split on a given value *)
  val split_on: t -> Bound_rat.t -> t list
  val split: t -> t list

  (** pruning *)
  val prune : (t -> t -> t list) option

  (************************************************************************)
  (** {1 INTERVAL ARITHMETICS (FORWARD EVALUATION)} *)
  (************************************************************************)

  val neg: t -> t
  val abs: t -> t

  val add: t -> t -> t
  val sub: t -> t -> t
  val mul: t -> t -> t

  (** return valid values (possibly Bot, if dividend is nul) *)
  val div: t -> t -> t bot

  (** returns valid value when the exponant is a singleton positive integer. fails otherwise*)
  val pow: t -> t -> t

  (** function calls (sqrt, exp, ln ...) are handled here :
     given a function name and and a list of argument,
     it returns a possibly bottom result *)
  val eval_fun : string -> t list -> t bot

  (************************************************************************)
  (** {1 FILTERING (TEST TRANSFER FUNCTIONS)}                             *)
  (************************************************************************)

  (** given two interval arguments, return a subset of each argument
      by removing points that cannot satisfy the predicate;
      may also return Bot if no point can satisfy the predicate
      simplified interface since a > b <=> b < a *)

  val filter_leq: t -> t -> (t * t) bot
  val filter_lt : t -> t -> (t * t) bot
  val filter_eq : t -> t -> (t * t) bot
  val filter_neq: t -> t -> (t * t) bot

  (** given the interval argument(s) and the expected interval result of
     a numeric operation, returns refined interval argument(s) where
     points that cannot contribute to a value in the result are
     removed;
     may also return Bot if no point in an argument can lead to a
     point in the result *)

  val filter_neg: t -> t -> t bot
  val filter_abs: t -> t -> t bot

  val filter_add: t -> t -> t -> (t*t) bot
  val filter_sub: t -> t -> t -> (t*t) bot
  val filter_mul: t -> t -> t -> (t*t) bot
  val filter_div: t -> t -> t -> (t*t) bot

  val filter_pow: t -> t -> t -> (t*t) bot

  (** filtering function calls like (sqrt, exp, ln ...) is done here :
     given a function name, a list of argument, and a result,
     it remove points that cannot satisfy the relation : f(arg1,..,argn) = r;
     it returns a possibly bottom result *)
  val filter_fun: string -> t list -> t -> (t list) bot

  (** Only filters the first argument *)
  val filter_add_f: t -> t -> t -> t bot
  val filter_sub_f: t -> t -> t -> t bot
  val filter_mul_f: t -> t -> t -> t bot
  val filter_div_f: t -> t -> t -> t bot
  val filter_pow_f: t -> t -> t -> t bot
  val filter_root_f: t -> t -> t -> t bot

  (** generate a random float within the given interval *)
  val spawn : t -> float

  (** shrinks each bound of the interval by the given value *)
  val shrink : t -> Bound_rat.t -> t bot
end
