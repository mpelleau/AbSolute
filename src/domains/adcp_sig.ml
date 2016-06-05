(**********************************************************************************)
(*            Module for Abstract Domains for Constraint Programming.             *)
(*   These are abstract domains with consistency, split and precision operators.  *)
(**********************************************************************************)

module type AbstractCP = sig

  (*** TYPES ***)
  (* abstract elements *)
  type t
  
  (*** INSTANCIATION ***)
   (* instanciation of an abstract element with the domains of the variables of a problem *)
   val of_problem : Syntax.prog -> t

   (*** PREDICATES ***)
   (* tests if an abstract element is too small to be cut *)
   val is_small : t -> bool

   val is_bottom : t -> bool

   val is_enumerated : t -> bool

   (*** OPERATIONS ***)
   val join: t -> t -> t

   (* pruning *)
   val prune : t -> t -> t list * t

   (* split an abstract element *)
   val split : t -> t list

   val filter : t -> (Syntax.expr * Syntax.cmpop * Syntax.expr) -> t

   val forward_eval : t -> Syntax.expr -> (float * float)

   (*** DRAWING AND PRINTING ***)
   (* drawing *)
   val points_to_draw : t -> (string * string) option-> (float * float) list
     
  (* printing *)
  val print : Format.formatter -> t -> unit

 end
