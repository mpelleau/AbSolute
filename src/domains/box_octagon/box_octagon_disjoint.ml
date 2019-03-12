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
    box_constraints: bconstraint list;
    reified_octagonal: reified_octagonal list;
  }

  let init box_vars octagon_vars initial_constraints reified_octagonal =
    let (constraints, octagon) = Octagon.init octagon_vars initial_constraints in
    let box_constraints = List.map snd (List.filter (fun (octagonal, _) -> not octagonal) constraints) in
    let box = Box.init box_vars in
    Octagon.closure octagon;
    {
      box_vars=box_vars;
      box=box;
      octagon=octagon;
      box_constraints=box_constraints;
      reified_octagonal=reified_octagonal;
    }

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
      let box = Box.meet_var b Box.I.one box_oct.box in
(*       Printf.printf "Entailed reified: %s <=> " b;
      List.iter (fun e -> Printf.printf "%s \\/ " (octagonal_to_string e)) conjunction;
      Printf.printf "\n"; *)
      { box_oct with box=box} end
    else if List.exists (fun e -> e = False) entailments then begin
      (* Printf.printf "c=false\n"; flush_all (); *)
(*       Printf.printf "Disentailed reified: %s <=> " b;
      List.iter (fun e -> Printf.printf "%s \\/ " (octagonal_to_string e)) conjunction;
      Printf.printf "\n"; *)
      let box = Box.meet_var b Box.I.zero box_oct.box in
      { box_oct with box=box} end
    else
      { box_oct with reified_octagonal=(b, conjunction)::box_oct.reified_octagonal }

  (** Filter all the reified octagonal constraints.
      See also `filter_reified_octagonal`. *)
  let reified_closure box_oct =
    List.fold_left filter_reified_octagonal
      {box_oct with reified_octagonal=[]}
      box_oct.reified_octagonal

  (** Closure of the box with regards to the box constraints.
      A fixed point is reached when no constraint can be propagated anymore. *)
  let box_closure volume box constraints =
    let rec aux volume box =
      let box = List.fold_left Box.closure box constraints in
      let volume' = Box.volume box in
      if volume' <> volume then
        aux volume' box
      else
        (volume, box) in
    aux volume box

  let remove_entailed_constraints box_oct =
    let is_unknown c =
      match Box.entailment box_oct.box c with
      | Unknown -> true
      | True -> false
      | False -> failwith "Found a constraint that is disentailed and Bot_found has not been raised." in
    { box_oct with box_constraints=List.filter is_unknown box_oct.box_constraints }

  (** This closure filters the box and octagon with regards to the (reified) constraints in `box_oct`.
      Besides reducing the domain of the variables, the entailed constraints are removed from `box_oct`. *)
  let closure (box_oct:t) = begin
    (* Printf.printf "Start Closure\n"; flush_all (); *)
    (* I. Connection between box and octagon by filtering the reified constraints. *)
    let box_oct = reified_closure box_oct in
    (* Printf.printf "Reified Closure Succeeded\n"; flush_all (); *)
    (* II. Performing the closure on the box. *)
    let (_,box) = box_closure (Box.volume box_oct.box) box_oct.box box_oct.box_constraints in
    (* Printf.printf "Box Closure Succeeded\n"; flush_all (); *)
    (* III. Removing the constraints that are entailed. *)
    remove_entailed_constraints { box_oct with box=box }
  end

  let split box_oct = begin
    let branches = List.map (fun octagon -> { box_oct with octagon=octagon }) (Octagon.split box_oct.octagon) in
    (* Printf.printf "Split %d\n" (List.length branches); flush_all (); *)
    branches
  end

  let volume box_oct =
    let box_vol = (Box.volume box_oct.box) in
    let oct_vol = (Octagon.volume box_oct.octagon) in
    if box_vol = 1. && oct_vol = 1. then 1.
    else if box_vol = 0. || oct_vol = 0. then 0.
    else box_vol +. oct_vol

  let state_decomposition box_oct =
    match Octagon.state_decomposition box_oct.octagon with
    | True -> if (List.length box_oct.reified_octagonal) = 0 && (List.length box_oct.box_constraints) = 0 then True else Unknown
    | False -> False
    | Unknown -> Unknown

  let project_one box_oct var =
    if List.mem var box_oct.box_vars then
      Box.I.to_range (Box.get box_oct.box var)
    else
      Octagon.project_one box_oct.octagon var

  let project box_oct vars = List.map (fun v -> (v, project_one box_oct v)) vars

  let meet_var box_oct var (l,u) =
    if B.lt u l then raise Bot.Bot_found;
    if List.mem var box_oct.box_vars then
      let itv = Box.I.of_bounds l u in
      { box_oct with box=Box.meet_var var itv box_oct.box }
    else
    begin
      let left = Rewriter.rewrite (Var var, LEQ, Cst (B.to_rat u, Int)) in
      let right = Rewriter.rewrite (Var var, GEQ, Cst (B.to_rat l, Int)) in
      List.iter (Octagon.incremental_closure box_oct.octagon) (left@right);
      box_oct
    end
end
