(* 
   Generic intervals.

   Can be instantiated with any bound type.
*)


open Bot
open Bound_sig
open Itv_sig


module type UITV = sig

  module I : ITV
  module B : BOUND
  type t = I.t
  type bound = I.bound

  val join : t list -> t list -> t list
  val union : t list -> t list -> t list option
  val meet : t list -> t list -> t list bot
  val min_max : t list -> t
  val to_hull : t list -> t

  (************************************************************************)
  (* PRINTING *)
  (************************************************************************)

  val to_string: t -> string
  val output: out_channel -> t -> unit
  val sprint: unit -> t -> string
  val bprint: Buffer.t -> t -> unit
  val pp_print: Format.formatter -> t -> unit
  val print: Format.formatter -> t -> unit

  (* predicates *)
  (* ---------- *)

  val equal: t list -> t list -> bool
  val subseteq: t list -> t list -> bool
  val contains: t list -> bound -> bool
  val intersect: t list -> t list  -> bool
  val is_bounded: t list -> bool
  val is_singleton: t list -> bool
  val check_bot: t -> t bot

  val simplify: t list -> t list

  (* mesure *)
  (* ------ *)

  val overlap: t -> t -> bound
  val range: t -> bound
  val magnitude: t -> bound

  (* split *)
  (* ----- *)

  val mean_list: t list -> bound list
  val split_list: t list -> bound list -> t  list bot
  val split_list_integer: t list -> bound list -> t list bot

  (* arithmetic *)
  (* ---------- *)

  val neg: t -> t
  val abs: t -> t
  val sqrt: t -> t bot

  val add: t -> t -> t
  val sub: t -> t -> t
  val mul: t -> t -> t

  (* return valid values (possibly Bot) + possible division by zero *)
  val div: t -> t -> t bot * bool
  val div2: t -> t -> t list bot
 
  (* returns valid value when the exponant is a singleton positive integer. fails otherwise*)  
  val pow: t -> t -> t
  val n_root: t -> t -> t bot

  val cos: t -> t
  val sin: t -> t
  val tan: t -> t
  val cot: t -> t

  val acos: t -> t bot
  val asin: t -> t bot
  val atan: t -> t
  val acot: t -> t

  val exp: t -> t
  val log: t -> t bot
  val log10: t -> t bot



  val filter_neg: t list -> t list -> t list bot
  val filter_abs: t list -> t list -> t list bot
  val filter_sqrt: t list -> t list -> t list bot

  val filter_add: t list -> t list -> t list -> (t list*t list) bot
  val filter_sub: t list -> t list -> t list -> (t list*t list) bot
  val filter_mul: t list -> t list -> t list -> (t list*t list) bot
  val filter_div: t list -> t list -> t list -> (t list*t list) bot

  val filter_cos: t list -> t list -> t list bot
  val filter_sin: t list -> t list -> t list bot
  (*val filter_tan: t -> t -> t bot
  val filter_asin: t -> t -> t bot
  val filter_acos: t -> t -> t bot
  val filter_atan: t -> t -> t bot
  val filter_exp: t -> t -> t bot
  val filter_log: t -> t -> t bot
  val filter_root: t -> t -> t -> t bot*)

end
