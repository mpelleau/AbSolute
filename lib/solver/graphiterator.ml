(** This module converts the constraints into an internal state and handles the
    order of filtering using a graph propagation scheme *)

open Signature
open Consistency

module Make (D : Domain) = struct
  type space = D.t

  type constraint_graph = (variable, constr) Cgraph.t

  and constr = D.internal_constr

  and variable = string

  let print_constr fmt c =
    Format.fprintf fmt "%a" Constraint.print (D.externalize c)

  let print_graph fmt g =
    Format.fprintf fmt "%a" (Cgraph.print Format.pp_print_string print_constr) g

  type t = {space: space; graph: constraint_graph; splits: Tools.VarSet.t}

  let init ?(verbose = false) (p : Csp.t) : t =
    if verbose then Format.printf "variable declaration ...%!" ;
    let space = List.fold_left D.add_var D.empty p.Csp.variables in
    if verbose then Format.printf " done.\n" ;
    if verbose then Format.printf "constraint conversion ...%!" ;
    let n = List.length p.variables in
    let constraints =
      List.map
        (fun c -> (Constraint.support c, D.internalize ~elem:space c))
        p.Csp.constraints
    in
    if verbose then Format.printf " done.\n%!" ;
    if verbose then Format.printf "graph building ...%!" ;
    let graph = Cgraph.build n constraints in
    if verbose then Format.printf " done.\n%!" ;
    if verbose then Format.printf "edges:@,%a\n%!" print_graph graph ;
    {space; graph; splits= Tools.VarSet.empty}

  (* graph propagation : each constraint is activated at most once *)
  let propagate {space; graph; splits} : t Consistency.t =
    let queue = Queue.create () in
    let activated = Hashtbl.create (Cgraph.nb_edges graph) in
    let add_to_queue c =
      if not (Hashtbl.mem activated c) then (
        Hashtbl.add activated c true ;
        Queue.add c queue )
    in
    let add_from v = Cgraph.iter_edges_from add_to_queue graph v in
    let rec loop graph sat abs =
      if Queue.is_empty queue then
        Filtered ({space= abs; graph; splits= Tools.VarSet.empty}, sat)
      else
        let c = Queue.pop queue in
        match D.filter_diff abs c with
        | Unsat -> Unsat
        | Sat ->
            let graph' = Cgraph.copy graph in
            Cgraph.remove_edge graph' c ;
            loop graph' sat abs
        | Filtered ((abs', _, diff), true) ->
            let graph' = Cgraph.copy graph in
            Cgraph.remove_edge graph' c ;
            Tools.VarSet.iter add_from diff ;
            loop graph' sat abs'
        | Filtered ((abs', _c', diff), false) ->
            Tools.VarSet.iter add_from diff ;
            loop graph false abs'
        | Pruned {sure; unsure} -> prune sat sure unsure
    and prune _sat _sure _unsure = failwith "pruning not implemented" in
    (* if no variable have been split, perform a full propagation *)
    if Tools.VarSet.is_empty splits then
      List.iter add_to_queue (Cgraph.get_edges graph)
    else Tools.VarSet.iter add_from splits ;
    loop graph true space

  let split ?prec e =
    let splits, diff = D.split_diff ?prec e.space in
    List.rev_map (fun space -> {e with space; splits= diff}) splits

  let spawn elm = D.spawn elm.space

  let to_result ~inner res elm =
    if inner then Result.add_inner res elm.space
    else Result.add_outer res elm.space

  let to_csp (elm : t) =
    let vars = D.vars elm.space in
    let cstrs = List.map D.externalize (Cgraph.get_edges elm.graph) in
    List.fold_left Csp.add_constr (Csp.initialize vars) cstrs
end
