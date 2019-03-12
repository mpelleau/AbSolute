open Box_dom
open Octagon
open Octagonal_rewriting
open Csp
open Abstract_domain

type reified_octagonal = (var * octagonal_constraint list)

module type Box_octagon_disjoint_sig =
sig
  module B: Bound_sig.BOUND
  type t
  type bound = B.t

  val init: var list -> var list -> bconstraint list -> reified_octagonal list -> t
  val closure: t -> t
  val split: t -> t list
  val volume: t -> float
  val state_decomposition: t -> kleene
  val project_one: t -> var -> (bound * bound)
  val project: t -> var list -> (var * (bound * bound)) list
  val meet_var: t -> var -> (bound * bound) -> t
end

module Make
  (B: Bound_sig.BOUND)
  (Octagon: Octagon_sig with type bound=B.t)
  (Box: Box_sig with type bound=B.t and type I.bound=B.t) =
struct
  module B = B
  type bound = B.t
  module Rewriter = Octagon.Rewriter

  type t = {
    box_vars: var list;
    box : Box.t;
    octagon: Octagon.t;
    reified_octagonal: reified_octagonal list;
  }

  let init box_vars octagon_vars initial_constraints reified_octagonal =
    let (constraints, octagon) = Octagon.init octagon_vars initial_constraints in
    let box_constraints = List.map snd (List.filter (fun (octagonal, _) -> not octagonal) constraints) in
    let box = Box.init box_vars box_constraints in
    Octagon.closure octagon;
    {
      box_vars=box_vars;
      box=box;
      octagon=octagon;
      reified_octagonal=reified_octagonal;
    }

  let volume box_oct =
    let box_vol = (Box.volume box_oct.box) in
    let oct_vol = (Octagon.volume box_oct.octagon) in
    if box_vol = 1. && oct_vol = 1. then 1.
    else if box_vol = 0. || oct_vol = 0. then 0.
    else box_vol +. oct_vol

  (** We filter the reified octagonal constraint.
      If the octagonal conjunction is entailed, we add `b=1` in the box.
      If it is disentailed, we add `b=0`.
      Otherwise, we re-insert this reified constraint in the box-octagon domain. *)
  let filter_reified_octagonal box_oct (b, conjunction) =
    (* I. b implies C *)
    let itv = Box.get box_oct.box b in
    if Box.I.is_singleton itv then
      let (value,_) = Box.I.to_range itv in
      (* When b=true, then we just add all the constraints in the closure. *)
      if B.equal B.one value then begin
        (* Printf.printf "b=true\n"; flush_all (); *)
        List.iter (Octagon.incremental_closure box_oct.octagon) conjunction;
        box_oct end
      (* When b=false, we look at the formula (not c_1 \/ ... \/ not c_n).
         We can only add "not c_i" if all other constraints "not c_j" are disentailed.
         Note that below we check "c_i" instead of "not c_i", this is why we consider it unsatisfiable if the constraint is entailed. *)
      else if B.equal B.zero value then
      begin
        let entailed = List.map (Octagon.entailment box_oct.octagon) conjunction in
        (* If one constraint is already false, then we cannot do anything, the reified constraint is already satisfiable. *)
        if List.exists (fun e -> e = False) entailed then
          (* (Printf.printf "All disentailed\n"; flush_all (); *)
          box_oct
        (* If all constraints are true, then the reified constraint is unsatisfiable. *)
        else if List.for_all (fun e -> e = True) entailed then
          (* (Printf.printf "All entailed\n"; flush_all (); *)
          raise Bot.Bot_found
        (* Otherwise, we can only negate the remaining "unknown" constraint, if it is single. *)
        else
          let count_unknown = List.fold_left (fun n e -> n + (if e = Unknown then 1 else 0)) 0 entailed in
          if count_unknown = 1 then
            let (_,unknown) = List.find (fun (e,c) -> e = Unknown) (List.combine entailed conjunction) in (
            (* Printf.printf "Rewriting %s into %s\n" (octagonal_to_string unknown) (octagonal_to_string (Rewriter.negate unknown));
            flush_all (); *)
            Octagon.incremental_closure box_oct.octagon (Rewriter.negate unknown); box_oct)
          else
            {box_oct with reified_octagonal=(b, conjunction)::box_oct.reified_octagonal}
      end
      else failwith "Reified boolean should be equal to 0 or 1."
    else
    (* II. C implies b  *)
    let entailments = List.map (Octagon.entailment box_oct.octagon) conjunction in
    if List.for_all (fun e -> e = True) entailments then begin
      (* Printf.printf "c=true\n"; flush_all (); *)
      let box = Box.weak_incremental_closure box_oct.box (Var b, EQ, constant_one) in
(*       Printf.printf "Entailed reified: %s <=> " b;
      List.iter (fun e -> Printf.printf "%s \\/ " (octagonal_to_string e)) conjunction;
      Printf.printf "\n"; *)
      { box_oct with box=box} end
    else if List.exists (fun e -> e = False) entailments then begin
      (* Printf.printf "c=false\n"; flush_all (); *)
(*       Printf.printf "Disentailed reified: %s <=> " b;
      List.iter (fun e -> Printf.printf "%s \\/ " (octagonal_to_string e)) conjunction;
      Printf.printf "\n"; *)
      let box = Box.weak_incremental_closure box_oct.box (Var b, EQ, constant_zero) in
      { box_oct with box=box} end
    else
      { box_oct with reified_octagonal=(b, conjunction)::box_oct.reified_octagonal }

  (** Filter all the reified octagonal constraints.
      See also `filter_reified_octagonal`. *)
  let reified_closure box_oct =
    List.fold_left filter_reified_octagonal
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
