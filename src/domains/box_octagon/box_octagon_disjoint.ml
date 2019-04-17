open Box_dom
open Octagon
open Csp
open Abstract_domain
open Dbm

module type Box_octagon_disjoint_sig =
sig
  module B: Bound_sig.BOUND
  type t
  type bound = B.t

  val init: var list -> var list -> bconstraint list -> Box_reified.box_reified_constraint list -> t
  val closure: t -> t
  val weak_incremental_closure: t -> bconstraint -> t
  val split: t -> t list
  val volume: t -> float
  val state_decomposition: t -> kleene
  val project_one: t -> var -> (bound * bound)
  val project: t -> var list -> (var * (bound * bound)) list
end

module Make
  (BOX: Box_functor)
  (Octagon: Octagon_sig) =
struct
  module B = Octagon.B
  module Box=BOX(B)
  type bound = B.t
  module Rewriter = Octagonal_rewriting.Rewriter(B)

  type reified_octagonal = var * (bound dbm_constraint) list

  type t = {
    box : Box.t;
    octagon: Octagon.t;
    rewriter: Rewriter.t;
    reified_octagonal: reified_octagonal list;
  }

  let init_octagon rewriter vars (octagon, constraints) c =
    if not (is_defined_over vars c) then octagon, c::constraints
    else
      match Rewriter.rewrite rewriter c with
      (* If we cannot rewrite the constraint, we add a relaxed version, if any.
         Note that the constraint is added to `constraints` anyway, so it can be fully processed by the box domain. *)
      | [] -> (List.fold_left Octagon.weak_incremental_closure octagon (Rewriter.relax rewriter c)), c::constraints
      | cons -> (List.fold_left Octagon.weak_incremental_closure octagon cons), constraints

  let init_reified_constraint rewriter (v, conjunction) =
    let try_rewrite all c =
      let rewritten_c = Rewriter.rewrite rewriter c in
      if (List.length rewritten_c)=0 then
        raise (Wrong_modelling ("The abstract domain `Box_octagon_disjoint` expects octagonal reified constraints, but `" ^
               (string_of_bconstraint c) ^ "` could not be rewritten."))
      else
        all@rewritten_c in
    (v, List.fold_left try_rewrite [] conjunction)

  let init box_vars octagon_vars constraints reified_octagonal =
    let dim = List.length octagon_vars in
    let rewriter = Rewriter.init (List.combine
      octagon_vars
      (Fold_intervals_canonical.fold (fun a itv -> itv::a) [] dim)) in
    let (octagon, constraints) =
      List.fold_left (init_octagon rewriter octagon_vars) (Octagon.init dim, []) constraints in
    let reified_octagonal = List.map (init_reified_constraint rewriter) reified_octagonal in
    let box = Box.init box_vars constraints in
    {
      box=box;
      octagon=octagon;
      rewriter=rewriter;
      reified_octagonal=reified_octagonal;
    }

  let volume box_oct =
    let box_vol = (Box.volume box_oct.box) in
    let oct_vol = (Octagon.volume box_oct.octagon) in
    if box_vol = 1. && oct_vol = 1. then 1.
    else if box_vol = 0. || oct_vol = 0. then 0.
    else box_vol +. oct_vol

  let entailment_of_reified box_oct conjunction =
    let entailed = List.map (Octagon.entailment box_oct.octagon) conjunction in
    and_reified entailed

  let propagate_negation_conjunction box_oct (b, conjunction) =
    match entailment_of_reified box_oct conjunction with
    | False, _ -> box_oct
    | True, _ -> raise Bot.Bot_found
    | Unknown, Some(u) ->
        let unknown = List.nth conjunction u in
        let neg_unknown = Rewriter.negate unknown in
        { box_oct with octagon=Octagon.weak_incremental_closure box_oct.octagon neg_unknown }
    | Unknown, None ->
        { box_oct with reified_octagonal=(b, conjunction)::box_oct.reified_octagonal }

  (* Propagate the reified constraints.
     Entailed reified constraints are removed from `box_oct`. *)
  let propagate_reified_octagonal box_oct (b, conjunction) =
    let itv = Box.get box_oct.box b in
    if Box.I.is_singleton itv then
      let (value,_) = Box.I.to_range itv in
      if B.equal B.one value then
        { box_oct with octagon=List.fold_left Octagon.weak_incremental_closure box_oct.octagon conjunction }
      else if B.equal B.zero value then
        propagate_negation_conjunction box_oct (b, conjunction)
      else failwith "Reified boolean should be equal to 0 or 1."
    else
      match fst (entailment_of_reified box_oct conjunction) with
      | False ->
       { box_oct with box=(Box.weak_incremental_closure box_oct.box (Var b, EQ, constant_zero)) }
      | True ->
       { box_oct with box=(Box.weak_incremental_closure box_oct.box (Var b, EQ, constant_one)) }
      | Unknown -> { box_oct with reified_octagonal=(b, conjunction)::box_oct.reified_octagonal }

  (** Filter all the reified octagonal constraints.
      See also `propagate_reified_octagonal`. *)
  let reified_closure box_oct =
    List.fold_left propagate_reified_octagonal
      {box_oct with reified_octagonal=[]}
      box_oct.reified_octagonal

  let rec propagate vol box_oct =
    let box_oct = reified_closure box_oct in
    let box_oct = { box_oct with box=Box.closure box_oct.box } in
    (* let box_oct = reified_closure box_oct in *)
    let box_oct = { box_oct with octagon=Octagon.closure box_oct.octagon } in
    let vol' = volume box_oct in
    if vol <> vol' then
      propagate vol' box_oct
    else
      box_oct

  let closure (box_oct:t) =
    (* Apply all the possible constraints from the splitting strategy. *)
    let box_oct = { box_oct with octagon=Octagon.closure box_oct.octagon } in
    propagate (volume box_oct) box_oct

  let weak_incremental_closure box_oct c =
    match Rewriter.rewrite box_oct.rewriter c with
    | [] -> { box_oct with box=Box.weak_incremental_closure box_oct.box c }
    | cons -> {box_oct with octagon=List.fold_left Octagon.weak_incremental_closure box_oct.octagon cons }

  let incremental_closure_octagon box_oct c = closure (weak_incremental_closure box_oct c)

  let split box_oct =
    let branches = List.map (fun octagon -> { box_oct with octagon=octagon }) (Octagon.split box_oct.octagon) in
    if (List.length branches) = 0 then
      let branches = Box.split box_oct.box in
      let octagons = Octagon.copy box_oct.octagon (List.length branches) in
      List.map2 (fun box octagon -> { box_oct with box=box; octagon=octagon }) branches octagons
    else
      branches

  let state_decomposition box_oct =
    match Octagon.state_decomposition box_oct.octagon, Box.state_decomposition box_oct.box with
    | True, True when (List.length box_oct.reified_octagonal) = 0 -> True
    | False, _ | _, False -> False
    | _ -> Unknown

  let project_one box_oct var =
    try
      Box.project_one box_oct.box var
    with Not_found ->
      Octagon.project box_oct.octagon (Rewriter.var_box_to_dbm box_oct.rewriter var)

  let project box_oct vars = List.map (fun v -> (v, project_one box_oct v)) vars
end
