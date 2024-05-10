(** This module converts the constraints into an internal state and handles the
    order of filtering using a graph propagation scheme *)

open Signature
open Consistency

module Make (D : Domain) = struct
  type space = D.t

  type constraint_graph = (variable, constr) Cgraph.t

  and constr = D.internal_constr

  and variable = string

  type t =
    { space: space
    ; graph: constraint_graph
    ; supports: (D.internal_constr, string list) Hashtbl.t
    ; splitted: Tools.VarSet.t }

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
    let supports = Hashtbl.create 1 in
    List.iter (fun (sup, c) -> Hashtbl.add supports c sup) constraints ;
    {space; graph; supports; splitted= Tools.VarSet.empty}

  let remove_constr graph supports (c : D.internal_constr) =
    let sup = Hashtbl.find_opt supports c in
    match sup with
    | Some sup ->
        Cgraph.(iter_edges (fun e -> remove_edge graph e c) sup) ;
        Hashtbl.remove supports c
    | None ->
        failwith
          (Format.asprintf "remove_constr %a" Constraint.print (D.externalize c))

  (* graph propagation : each constraint is activated at most once *)
  let propagate {space; graph; supports; splitted} : t Consistency.t =
    let queue = Queue.create () in
    let nb_constr = Hashtbl.length supports in
    let activated = Hashtbl.create nb_constr in
    let add_to_queue =
      Cgraph.iter_edges_from
        (fun c ->
          if not (Hashtbl.mem activated c) then (
            Hashtbl.add activated c true ;
            Queue.add c queue ) )
        graph
    in
    let rec loop sat abs =
      if Queue.is_empty queue then
        Filtered
          ({space= abs; graph; supports; splitted= Tools.VarSet.empty}, sat)
      else
        let c = Queue.pop queue in
        match D.filter_diff abs c with
        | Unsat -> Unsat
        | Sat ->
            remove_constr graph supports c ;
            loop sat abs
        | Filtered ((abs', _, diff), true) ->
            remove_constr graph supports c ;
            Tools.VarSet.iter add_to_queue diff ;
            loop sat abs'
        | Filtered ((abs', _c', diff), false) ->
            Tools.VarSet.iter add_to_queue diff ;
            loop false abs'
        | Pruned {sure; unsure} -> prune sat sure unsure
    and prune _sat _sure _unsure = failwith "pruning not implemented" in
    (* if no variable have been splitted, perform a full propagation *)
    if Tools.VarSet.is_empty splitted then
      List.iter (fun (_, v, _) -> add_to_queue v) (D.vars space)
    else Tools.VarSet.iter add_to_queue splitted ;
    loop true space

  let split ?prec e =
    List.rev_map
      (fun space ->
        { e with
          space
        ; graph= Cgraph.copy e.graph
        ; supports= Hashtbl.copy e.supports } )
      (D.split ?prec e.space)

  let spawn elm = D.spawn elm.space

  let to_result ~inner res elm =
    if inner then Result.add_inner res elm.space
    else Result.add_outer res elm.space

  let constraints graph = Cgraph.fold_edges ~duplicate:false List.cons [] graph

  let to_csp (elm : t) =
    let vars = D.vars elm.space in
    let cstrs = List.map D.externalize (constraints elm.graph) in
    List.fold_left Csp.add_constr (Csp.initialize vars) cstrs
end
