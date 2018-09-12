(*
  This module is wrapper that provides utilities to handle unconstrained
  variables: It is built upon a regular domain and can be used just as a
  regular domain
 *)
open Tools
open Adcp_sig

module Make(Abs:AbstractCP) = struct

  type t = {elm:Abs.t; unconstrained:Mpqf.t VarMap.t}

  let empty = {elm=Abs.empty; unconstrained=VarMap.empty}

  (* returns the variables *)
  let vars (t1:t) : (Csp.annot * Csp.var) list =
    (* TODO: replace the "failwith" with your own code *)
    failwith "value 'vars' in file 'unconstrainted.ml' not implemented"

  (* adds an unconstrained variable to the environnement *)
  let add_var elm ((ant:Csp.annot),(v:Csp.var)) =
    failwith "value 'add_var' in file 'unconstrainted.ml' not implemented"

  (* returns the bounds of a variable *)
  let var_bounds (t1:t) (v:Csp.var) : (Mpqf.t * Mpqf.t) =
    (* TODO: replace the "failwith" with your own code *)
    failwith "value 'var_bounds' in file 'unconstrainted.ml' not implemented"

  (* returns the bound variables *)
  let bound_vars (t1:t) : Csp.csts =
    (* TODO: replace the "failwith" with your own code *)
    failwith "value 'bound_vars' in file 'unconstrainted.ml' not implemented"

  (* removes an unconstrained variable from the environnement *)
  let rem_var (t1:t) (v:Csp.var) : t =
    (* TODO: replace the "failwith" with your own code *)
    failwith "value 'rem_var' in file 'unconstrainted.ml' not implemented"

  (*** PREDICATES ***)

  (* tests if an abstract element is too small to be cut *)
  let is_small (t1:t) : bool =
    (* TODO: replace the "failwith" with your own code *)
    failwith "value 'is_small' in file 'unconstrainted.ml' not implemented"

  let is_empty (t1:t) : bool =
    (* TODO: replace the "failwith" with your own code *)
    failwith "value 'is_empty' in file 'unconstrainted.ml' not implemented"

  (*** OPERATIONS ***)
  let join (t1:t) (t2:t) : t =
    (* TODO: replace the "failwith" with your own code *)
    failwith "value 'join' in file 'unconstrainted.ml' not implemented"

  (* pruning *)
  let prune (t1:t) (t2:t) : t list * t =
    (* TODO: replace the "failwith" with your own code *)
    failwith "value 'prune' in file 'unconstrainted.ml' not implemented"

  (* splits an abstract element *)
  let split (t1:t) : t list =
    (* TODO: replace the "failwith" with your own code *)
    failwith "value 'split' in file 'unconstrainted.ml' not implemented"

  let filter (t:t) ((e1:Csp.expr),(cmp:Csp.cmpop), (e2:Csp.expr)) =
    failwith "value 'filter' in file 'unconstrainted.ml' not implemented"

  let forward_eval (t1:t) (e:Csp.expr) : (Mpqf.t * Mpqf.t) =
    (* TODO: replace the "failwith" with your own code *)
    failwith "value 'forward_eval' in file 'unconstrainted.ml' not implemented"

  (* transforms an abstract element in constraints *)
  let to_bexpr (t1:t) : (Csp.expr * Csp.cmpop * Csp.expr) list =
    (* TODO: replace the "failwith" with your own code *)
    failwith "value 'to_bexpr' in file 'unconstrainted.ml' not implemented"

  (* check if a constraint is suited for this abstract domain *)
  let is_representable (b:Csp.bexpr) : answer =
    (* TODO: replace the "failwith" with your own code *)
    failwith "value 'is_representable' in file 'unconstrainted.ml' not implemented"

  (* printing *)
  let print (fmt:Format.formatter) (t2:t) : unit =
    (* TODO: replace the "failwith" with your own code *)
    failwith "value 'print' in file 'unconstrainted.ml' not implemented"

  (* volume *)
  let volume (t1:t) : float =
    (* TODO: replace the "failwith" with your own code *)
    failwith "value 'volume' in file 'unconstrainted.ml' not implemented"

  (* concretization function. we call it a spawner.
     useful to do tests, and to reuse the results.
     values are generated randomly *)
  let spawn (t1:t) : Csp.instance =
    (* TODO: replace the "failwith" with your own code *)
    failwith "value 'spawn' in file 'unconstrainted.ml' not implemented"

  (* check if an abstract element is an abstractin of an instance *)
  let is_abstraction (t1:t) (i:Csp.instance) : bool =
    (* TODO: replace the "failwith" with your own code *)
    failwith "value 'is_abstraction' in file 'unconstrainted.ml' not implemented"
end
