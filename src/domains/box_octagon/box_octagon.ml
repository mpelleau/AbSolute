open Box_dom
open Octagon
open Octagonal_rewriting
open Interval_view_dbm
open Csp
open Octagonalisation
open Abstract_domain

type reified_octagonal = (var * octagonal_constraint list)

module type Box_octagon_sig =
sig
  type t
  type bound

  val init: var list -> var list -> bconstraint list -> reified_octagonal list -> t
end

module Make
  (B: Bound_sig.BOUND)
  (RotationStrategy: Octagonalisation)
  (Octagon: Octagon_sig with type bound=B.t)
  (Box: Box_sig with type bound=B.t and type I.bound=B.t) =
struct

  include RotationStrategy

  module Env = Tools.VarMap
  module REnv = Mapext.Make(struct
    type t=dbm_key
    let compare = compare end)

  type bound = B.t

  type t = {
    (* initial variables in the box. *)
    box_vars: var list;
    box : Box.t;
    octagon: Octagon.t;
    constraints: bconstraint list;
    reified_octagonal: reified_octagonal list;
    (* maps each rotated variable name to its `key` in the octagon. *)
    env: dbm_key Env.t;
    (* reversed mapping of `env`. *)
    renv: string REnv.t;
  }

  let init box_vars octagon_vars constraints reified_octagonal =
    let (constraints, octagon) = Octagon.init octagon_vars constraints in
    let box_constraints = List.map snd (List.filter (fun (octagonal, _) -> not octagonal) constraints) in
    let (rotated_constraints, rotated_vars) = rotate octagon_vars box_constraints in
    let box = Box.init (box_vars@List.map fst rotated_vars) in
    let env_add env (name, key) = Env.add name key env in
    let renv_add renv (name, key) = REnv.add key name renv in
    {
      box_vars=box_vars;
      box=box;
      octagon=octagon;
      constraints=box_constraints@rotated_constraints;
      reified_octagonal=reified_octagonal;
      env=List.fold_left env_add Env.empty rotated_vars;
      renv=List.fold_left renv_add REnv.empty rotated_vars;
    }

  (** We filter the reified octagonal constraint.
      If the octagonal conjunction is entailed, we add `b=1` in the box.
      If it is disentailed, we add `b=0`.
      Otherwise, we re-insert this reified constraint in the box-octagon domain. *)
  let filter_reified_octagonal box_oct (b, conjunction) =
    let entailments = List.map (Octagon.entailment box_oct.octagon) conjunction in
    if List.for_all (fun e -> e = True) entailments then
      let box = Box.meet_var b Box.I.one box_oct.box in
      { box_oct with box=box}
    else if List.exists (fun e -> e = False) entailments then
      let box = Box.meet_var b Box.I.zero box_oct.box in
      { box_oct with box=box}
    else
      { box_oct with reified_octagonal=(b, conjunction)::box_oct.reified_octagonal }

  (** Filter all the reified octagonal constraints.
      See also `filter_reified_octagonal`. *)
  let reified_closure box_oct =
    List.fold_left filter_reified_octagonal
      {box_oct with reified_octagonal=[]}
      box_oct.reified_octagonal

  (** Closure of the box with regards to the box constraints.
      A fix point is reached when no constraint can be propagated anymore. *)
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
    let is_unknown c = (Box.entailment box_oct.box c) = Unknown in
    { box_oct with constraints=List.filter is_unknown box_oct.constraints }

  (* We update the DBM with the values contained in the box if they improve the current bound in the DBM. *)
  let meet_box_in_dbm rotated_vars box octagon =
    let update_cell_from_box var_name key =
      let itv = Box.get box var_name in
      let (l, u) = Box.I.to_range itv in
      Octagon.set_lb octagon key l;
      Octagon.set_ub octagon key u;
    in
    Octagon.iter_vars update_cell_from_box octagon;
    Env.iter update_cell_from_box rotated_vars;
    octagon

  let meet_dbm_in_box rotated_vars octagon box =
    let update_box var_name key box =
      let l = Octagon.lb octagon key in
      let u = Octagon.ub octagon key in
      if B.gt l u then failwith "meet_dbm_in_box: l > u";
      let i = Box.I.of_bounds l u in
      Box.meet_var var_name i box in
    let box = Octagon.fold_vars update_box box octagon in
    Env.fold update_box rotated_vars box

  let rec cheap_closure volume box_oct =
    let box_oct = reified_closure box_oct in
    let (volume', box) = box_closure volume box_oct.box box_oct.constraints in
    if volume <> volume' then
      cheap_closure volume' { box_oct with box=box }
    else
      (volume, box_oct)

  (** This closure filters the box and octagon with regards to the (reified) constraints in `box_oct`.
      Besides reducing the domain of the variables, the entailed constraints are removed from `box_oct`. *)
  let closure box_oct =
    let initial_volume = Box.volume box_oct.box in
    let (volume', box_oct) = cheap_closure initial_volume box_oct in
    if initial_volume <> volume' then
      let box_oct = remove_entailed_constraints box_oct in
      let box_oct = {box_oct with octagon=meet_box_in_dbm box_oct.env box_oct.box box_oct.octagon} in
      Octagon.closure box_oct.octagon;
      { box_oct with
        box=meet_dbm_in_box box_oct.env box_oct.octagon box_oct.box; }
    else
      box_oct

  (* WARNING: This strategy is only valid for integers. (This should be corrected when we extract the splitting strategy). *)
  (* Input-order selection of variables with assignment to the lower bound of the variable. *)
  let split box_oct =
    let var = List.find (fun v -> not (Box.I.is_singleton (Box.get box_oct.box v))) box_oct.box_vars in
    let (l,u) = Box.I.to_range (Box.get box_oct.box var) in
    let left_box = Box.meet_var var (Box.I.of_bounds l l) box_oct.box in
    let right_box = Box.meet_var var (Box.I.of_bounds (B.add_up l B.one) u) box_oct.box in
    (* We perform `meet_box_in_dbm` before the octagon closure so the octagon will be automatically updated with this new bound. *)
    [
      { box_oct with box=left_box };
      { box_oct with box=right_box; octagon=(Octagon.copy box_oct.octagon)}
    ]

  let volume box_oct = Box.volume (Box.project (fun x -> List.mem x box_oct.box_vars) box_oct.box)

  let state_decomposition box_oct =
    if volume box_oct = 0. then
      False
    else if (List.length box_oct.constraints) = 0 && (List.length box_oct.reified_octagonal) = 0 then
      True
    else
      Unknown
end
