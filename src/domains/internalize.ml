(** This wrapper internalizes the representation on the constraint to make
    it domain dependant. It allows to permform transformations on the problem
    at initialization *)

module Make(D:Signature.AbstractCP) = struct
  include Boolean.Make(D)

  type t = {
      search_space  : D.t;
      constraints   : Csp.constrs;
    }

  let init (prob:Csp.prog) =
    let search_space = List.fold_left (fun acc (ann,name,_) ->
                           D.add_var acc (ann,name)) D.empty prob.Csp.init
    in
    let constraints = prob.Csp.constraints in
    {search_space; constraints}

  (** returns the variables annoted by their type *)
  let vars {search_space; _} = D.vars search_space

  (** returns the bounds of a variable *)
  let var_bounds {search_space; _} = D.var_bounds search_space

  (** returns the bound variables *)
  let bound_vars {search_space; _} = D.bound_vars search_space

  (** removes an unconstrained variable from the environnement *)
  let rem_var {search_space; _} = D.rem_var search_space

  (*** PREDICATES ***)

  (** tests if an abstract element is small enough with respect to `Constant.precision` *)
  let is_small {search_space; _} = D.is_small search_space

  (** tests if an abstract element is empty *)
  let is_empty {search_space; _} = D.is_empty search_space


  (** splits an abstract element *)
  let split {search_space; constraints} ctrs =
    List.rev_map (fun sp -> {search_space=sp; constraints}) (D.split search_space ctrs)

  (** Pizza splits an abstract element around the given point *)
  let split_on {search_space; constraints} ctrs instance =
    List.rev_map (fun sp -> {search_space=sp; constraints})
      (D.split_on search_space instance ctrs)

  (** filters an abstract element *)
  let filter {search_space; constraints} =
    let abs' = List.fold_left (fun a c -> filter a c) search_space constraints in
    {search_space=abs'; constraints}

  (** returns the range of value of a given expression for an abstract element *)
  let forward_eval {search_space; _} =  D.forward_eval search_space

  (** transforms an abstract element into constraints *)
  let to_bexpr {search_space; _} = D.to_bexpr search_space

  (** printing *)
  let print fmt {search_space; _} : unit = D.print fmt search_space

  (** computes the volume of an abstract element *)
  let volume {search_space; _} = D.volume search_space

  (** concretization function. we call it a spawner.
      useful to do tests, and to reuse the results.
      values are generated randomly *)
  let spawn {search_space; _} = D.spawn search_space

  (** check if an abstract element is an abstraction of an instance *)
  let is_abstraction {search_space; _} = D.is_abstraction search_space

  (** Skrinks the abstract element in every direction by the given value. *)
  let shrink {search_space; _} = D.shrink search_space

end
