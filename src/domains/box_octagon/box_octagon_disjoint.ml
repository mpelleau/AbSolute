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
  (Box: Box_sig)
  (Octagon: Octagon_sig with module B=Box.I.B) =
struct
  module B = Box.I.B
  type bound = B.t
  module Rewriter = Octagonal_rewriter.Rewriter(B)

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
      match rewrite c with
      | [] -> (List.fold_left Octagon.weak_incremental_closure octagon (relax c)), c::constraints
      | cons -> (List.fold_left Octagon.weak_incremental_closure octagon cons), constraints

  let init box_vars octagon_vars constraints reified_octagonal =
    let dim = List.length octagon_vars in
    let rewriter = Rewriter.init (List.combine
      octagon_vars
      (Fold_intervals_canonical.fold (fun a itv -> itv::a) [] dim)) in
    let (octagon, constraints) =
      List.fold_left (init_octagon rewriter octagon_vars) (Octagon.init dim, []) constraints in
    let box = Box.init box_vars constraints in
    {
      box=box;
      octagon=octagon;
      rewriter=rewriter;
      reified_octagonal=reified_octagonal;
    }

TODO: continue here with the others functions...

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
        let neg_unknown = Rewriter.negate (List.nth conjunction u) in
        (Octagon.incremental_closure box_oct.octagon neg_unknown; box_oct)
    | Unknown, None ->
        {box_oct with reified_octagonal=(b, conjunction)::box_oct.reified_octagonal}

  (* Propagate the reified constraints.
     Entailed reified constraints are removed from `box_oct`. *)
  let propagate_reified_octagonal box_oct (b, conjunction) =
    let itv = Box.get box_oct.box b in
    if Box.I.is_singleton itv then
      let (value,_) = Box.I.to_range itv in
      if B.equal B.one value then
        (List.iter (Octagon.incremental_closure box_oct.octagon) conjunction; box_oct)
      else if B.equal B.zero value then
        propagate_negation_conjunction box_oct (b, conjunction)
      else failwith "Reified boolean should be equal to 0 or 1."
    else
      match fst (entailment_of_reified box_oct conjunction) with
      | False -> { box_oct with box=(Box.weak_incremental_closure box_oct.box (Var b, EQ, constant_zero)) }
      | True -> { box_oct with box=(Box.weak_incremental_closure box_oct.box (Var b, EQ, constant_one)) }
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
    let vol' = volume box_oct in
    if vol <> vol' then
      propagate vol' box_oct
    else
      box_oct

  let closure (box_oct:t) = propagate (volume box_oct) box_oct

  let split box_oct = begin
    let branches = List.map (fun octagon -> { box_oct with octagon=octagon }) (Octagon.split box_oct.octagon) in
    (* Printf.printf "Split %d\n" (List.length branches); flush_all (); *)
    branches
  end

  let state_decomposition box_oct =
    match Octagon.state_decomposition box_oct.octagon, Box.state_decomposition box_oct.box with
    | True, True ->
        if (List.length box_oct.reified_octagonal) = 0 then
          failwith "box_octagon_disjoint.state_decomposition: Found a reified constraint that is not entailed but both the box and octagon are entailed."
        else True
    | False, _ | _, False -> False
    | _ -> Unknown

  let project_one box_oct var =
    if List.mem var box_oct.box_vars then
      Box.I.to_range (Box.get box_oct.box var)
    else
      Octagon.project_one box_oct.octagon var

  let project box_oct vars = List.map (fun v -> (v, project_one box_oct v)) vars

(* NOTE: we need weak_incremental_closure. *)
(*   let incremental_closure_octagon box_oct c =
    let oct_constraints = Rewriter.rewrite c in
    if (List.length oct_constraints) = 0 then
      failwith "incremental_closure_octagon: the constraint `c` can not be rewritten as octagonal constraints."
    else
      List.iter (Octagon.incremental_closure box_oct octagon)

  let incremental_closure_box box_oct c =
    { box_oct with box=Box.incremental_closure box_oct.box c } *)

  let meet_var box_oct var (l,u) =
    if B.lt u l then raise Bot.Bot_found;
    let lb_cons = (Var var, LEQ, Cst (B.to_rat u, Int)) in
    let ub_cons = (Var var, GEQ, Cst (B.to_rat l, Int)) in
    if List.mem var box_oct.box_vars then
      let box = List.fold_left Box.weak_incremental_closure box_oct.box [lb_cons; ub_cons] in
      { box_oct with box=Box.closure box }
    else
    begin
      let left = Rewriter.rewrite lb_cons in
      let right = Rewriter.rewrite ub_cons in
      List.iter (Octagon.incremental_closure box_oct.octagon) (left@right);
      box_oct
    end
end
