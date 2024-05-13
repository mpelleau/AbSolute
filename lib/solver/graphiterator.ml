(** This module converts the constraints into an internal state and handles the
    order of filtering using a graph propagation scheme *)

open Signature
open Consistency
open Tools

module Make (D : Domain) = struct
  type space = D.t

  type constr = {original: Constraint.t; internal: D.internal_constr; id: int}

  type constraint_graph = (variable, constr) Cgraph.t

  and variable = string

  let print_constr fmt c = Format.fprintf fmt "%a" Constraint.print c.original

  let print_graph fmt g =
    Format.fprintf fmt "%a" (Cgraph.print Format.pp_print_string print_constr) g

  type t = {space: space; graph: constraint_graph; splits: VarSet.t}

  let init ?(verbose = false) (p : Csp.t) : t =
    if verbose then Format.printf "variable declaration ...%!" ;
    let space = List.fold_left D.add_var D.empty p.Csp.variables in
    if verbose then Format.printf " done.\n" ;
    if verbose then Format.printf "constraint conversion ...%!" ;
    let n = List.length p.variables in
    let supports =
      List.mapi
        (fun i c ->
          ( Constraint.support c
          , {original= c; internal= D.internalize ~elem:space c; id= i} ) )
        p.Csp.constraints
    in
    if verbose then Format.printf " done.\n%!" ;
    if verbose then Format.printf "graph building ...%!" ;
    let graph = Cgraph.build n supports in
    if verbose then Format.printf " done.\n%!" ;
    if verbose then Format.printf "%a\n%!" print_graph graph ;
    {space; graph; splits= VarSet.empty}

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
        Filtered ({space= abs; graph; splits= VarSet.empty}, sat)
      else
        let c = Queue.pop queue in
        match D.filter_diff abs c.internal with
        | Unsat -> Unsat
        | Sat ->
            let graph' = Cgraph.subset graph c in
            loop graph' sat abs
        | Filtered ((abs', _, diff), true) ->
            let graph' = Cgraph.subset graph c in
            VarSet.iter add_from diff ; loop graph' sat abs'
        | Filtered ((abs', _, diff), false) ->
            VarSet.iter add_from diff ; loop graph false abs'
        | Pruned {sure; unsure} -> prune sat sure unsure
    and prune _sat _sure _unsure = failwith "pruning not implemented" in
    (* if no variable have been split, perform a full propagation *)
    if VarSet.is_empty splits then
      List.iter add_to_queue (Cgraph.get_edges graph)
    else VarSet.iter add_from splits ;
    loop graph true space

  (* splits and update the set of split variables *)
  let split ?prec e =
    let splits, diff = D.split_diff ?prec e.space in
    List.rev_map (fun space -> {e with space; splits= diff}) splits

  let spawn elm = D.spawn elm.space

  let to_result ~inner res elm =
    if inner then Result.add_inner res elm.space
    else Result.add_outer res elm.space

  let to_csp (elm : t) =
    let vars = D.vars elm.space in
    let cstrs =
      List.map (fun c -> D.externalize c.internal) (Cgraph.get_edges elm.graph)
    in
    List.fold_left Csp.add_constr (Csp.initialize vars) cstrs
end
