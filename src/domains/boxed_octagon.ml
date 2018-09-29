(* This file implements the abstract domain of Octagon.
   It relies on the technique described in the dissertation of Marie Pelleau, Chapter 5 (2012).
   In particular, it is based on the observation that a 2D octagon can be represented by the intersection of two boxes, where one box is turned at 45° with respect to the other.
   It generalizes to dimension N with N^2 boxes (every box on (i,j)-plan is turned).
   This is useful because usual interval constraint propagators can be used.
   In addition, we have additional pruning with the Floyd Warshall algorithm filtering the octagonal constraints.
*)

open Adcp_sig
open Tools

let fail () = Pervasives.failwith "BoxedOctagon: function unimplemented."

module BoxedOctagon = struct
  type t = unit

  (* returns an empty element *)
  let empty : t = ()

  (* returns the variables *)
  let vars : t -> (Csp.annot * Csp.var) list
    = fun o -> []

  (* adds an unconstrained variable to the environnement *)
  let add_var : t -> Csp.annot * Csp.var -> t
    = fun o var -> ()

  (* returns the bounds of a variable *)
  let var_bounds : t -> Csp.var -> (Mpqf.t * Mpqf.t)
    = fun o var -> fail ()

  (* returns the bound variables *)
  let bound_vars : t -> Csp.csts
    = fun o -> []

  (* removes an unconstrained variable from the environnement *)
  let rem_var : t -> Csp.var -> t
    = fun o var -> ()

  (*** PREDICATES ***)

  (* tests if an abstract element is too small to be cut *)
  let is_small : t -> bool
    = fun o -> true

  let is_empty : t -> bool
    = fun o -> true

  (*** OPERATIONS ***)
  let join : t -> t -> t
    = fun o o' -> o

  (* pruning *)
  let prune : t -> t -> t list * t
    = fun o o' -> fail ()

  (* splits an abstract element *)
  let split : t -> t list
    = fun o -> []

  let filter : t -> (Csp.expr * Csp.cmpop * Csp.expr) -> t
    = fun o cons -> o

  let forward_eval : t -> Csp.expr -> (Mpqf.t * Mpqf.t)
    = fun o e -> fail ()

  (* transforms an abstract element in constraints *)
  let to_bexpr : t -> (Csp.expr * Csp.cmpop * Csp.expr) list
    = fun o -> []

  (* check if a constraint is suited for this abstract domain *)
  let is_representable : Csp.bexpr -> answer
    = fun b -> Adcp_sig.No

  (* printing *)
  let print : Format.formatter -> t -> unit
    = fun fmt o -> ()

  (* volume *)
  let volume : t -> float
    = fun o -> 0.

  (* concretization function. we call it a spawner.
     useful to do tests, and to reuse the results.
     values are generated randomly *)
  let spawn : t -> Csp.instance
    = fun o -> VarMap.empty

  (* check if an abstract element is an abstractin of an instance *)
  let is_abstraction : t -> Csp.instance -> bool
    = fun o vars -> false

end

(* module BoxedOctagonF       = BoxedOctagon(Trigo.Make(Itv.ItvF))
module BoxedOctagonStrict  = BoxedOctagon(Trigo.Make(Newitv.Test))
module BoxedOctagonQ       = BoxedOctagon(Trigo.Make(Itv.ItvQ))
module BoxedOctagonQStrict = BoxedOctagon(Trigo.Make(Newitv.TestQ))
module BoxedOctagonMix     = BoxedOctagon(Trigo.Make(Itv_mix)) *)
