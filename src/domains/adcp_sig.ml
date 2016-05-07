(**********************************************************************************)
(*            Module for Abstract Domains for Constraint Programming.             *)
(*   These are abstract domains with consistency, split and precision operators.  *)
(**********************************************************************************)

module type AbstractCP = sig

  (*** TYPES ***)
  (* abstract elements *)
  type t
  
  (* values used to split an abstract element *)
  type split

  (*** INSTANCIATION ***)
   (* instanciation of an abstract element with the domains of the variables of a problem *)
   val of_problem : Syntax.prog -> t

   (*** PREDICATES ***)
   (* tests if an abstract element is too small to be cut *)
   val is_small : t -> float -> (bool * split list)

   val is_bottom : t -> bool

   val sat_cons : t -> Syntax.bexpr -> bool

   (*** OPERATIONS ***)
   (* split an abstract element according to the split list *)
   val split : t -> split list -> t list

   val meet : t -> Syntax.bexpr -> t

   val forward_eval : t -> Syntax.expr -> (float * float)



   (*** DRAWING AND PRINTING ***)
   (* drawing *)
   val points_to_draw : t -> (string * string) option-> (float * float) list
     
  (* printing *)
  val print : Format.formatter -> t -> unit

 end
