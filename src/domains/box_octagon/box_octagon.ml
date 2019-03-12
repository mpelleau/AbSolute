(* open Box_dom
open Octagon
open Octagonal_rewriting
open Interval_view_dbm
open Csp
open Octagonalisation
open Abstract_domain

type reified_octagonal = (var * octagonal_constraint list)

module type Box_octagon_sig =
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
  (RotationStrategy: Octagonalisation)
  (Octagon: Octagon_sig with type bound=B.t)
  (Box: Box_sig with type bound=B.t and type I.bound=B.t) =
struct

  include RotationStrategy

  module Env = Tools.VarMap
  module REnv = Mapext.Make(struct
    type t=dbm_key
    let compare = compare end)

  module B = B
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

  (** We filter the reified octagonal constraint.
      If the octagonal conjunction is entailed, we add `b=1` in the box.
      If it is disentailed, we add `b=0`.
      Otherwise, we re-insert this reified constraint in the box-octagon domain. *)
  let filter_reified_octagonal box_oct (b, conjunction) =
    let entailments = List.map (Octagon.entailment box_oct.octagon) conjunction in
    if List.for_all (fun e -> e = True) entailments then begin
      (* Printf.printf "Entailed reified: %s <=> " b;
      List.iter (fun e -> Printf.printf "%s \\/ " (octagonal_to_string e)) conjunction;
      Printf.printf "\n"; *)
      let box = Box.weak_incremental_closure box_oct.box (Var b, EQ, constant_one) in
      { box_oct with box=box} end
    else if List.exists (fun e -> e = False) entailments then begin
      (* Printf.printf "Disentailed reified: %s <=> " b;
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

  (** Closure of the box with regards to the box constraints.
      A fix point is reached when no constraint can be propagated anymore. *)
  let box_closure volume box constraints =
    let rec aux volume box =
      let box = List.fold_left Box.incremental_closure box constraints in
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
      | True -> ((* let (e1,op,e2) = c in Format.printf "Entailed constraint %a \n" print_bexpr (Cmp (op,e1,e2));  *)false)
      | False -> ((* let (e1,op,e2) = c in Format.printf "Disentailed constraint %a \n" print_bexpr (Cmp (op,e1,e2)); *) false) in
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
  let closure (box_oct:t) =
    let one_pass box_oct =
      let initial_volume = Box.volume box_oct.box in
      let (volume', box_oct) = cheap_closure initial_volume box_oct in
      let box_oct = remove_entailed_constraints box_oct in
      let box_oct = {box_oct with octagon=meet_box_in_dbm box_oct.env box_oct.box box_oct.octagon} in
      Octagon.closure box_oct.octagon;
      { box_oct with
        box=meet_dbm_in_box box_oct.env box_oct.octagon box_oct.box; } in
    let initial_volume = Box.volume box_oct.box in
    let box_oct = one_pass box_oct in
    let volume' = Box.volume box_oct.box in
    if initial_volume <> volume' then one_pass box_oct else box_oct
    (* Printf.printf "Before closure %d %d \n" (List.length box_oct.constraints) (List.length box_oct.reified_octagonal); *)
    (* Printf.printf "After closure %d %d \n" (List.length box_oct.constraints) (List.length box_oct.reified_octagonal); *)

   let input_order_var box vars =
     try Some(List.find (fun v -> not (Box.I.is_singleton (Box.get box v))) vars)
     with Not_found -> None

  (* (* WARNING: This strategy is only valid for integers. (This should be corrected when we extract the splitting strategy). *)
  (* Input-order selection of variables with assignment to the lower bound of the variable. *)
  let split box_oct =
    match input_order_var box_oct.box box_oct.box_vars with
    | None -> []
    | Some(var) ->
        let (l,u) = Box.I.to_range (Box.get box_oct.box var) in
        let left_box = Box.meet_var var (Box.I.of_bounds l l) box_oct.box in
        let right_box = Box.meet_var var (Box.I.of_bounds (B.add_up l B.one) u) box_oct.box in
        (* We perform `meet_box_in_dbm` before the octagon closure so the octagon will be automatically updated with this new bound. *)
        [
          { box_oct with box=left_box };
          { box_oct with box=right_box; octagon=(Octagon.copy box_oct.octagon) }
        ] *)

  let split box_oct =
    List.map (fun octagon -> {box_oct with octagon=octagon; box=meet_dbm_in_box box_oct.env octagon box_oct.box; })
      (Octagon.split box_oct.octagon)

  let volume box_oct = Box.volume (Box.project (fun x -> List.mem x box_oct.box_vars) box_oct.box)

  let state_decomposition box_oct =
    if volume box_oct = 0. then
      False
    else
      match input_order_var box_oct.box box_oct.box_vars with
      | None -> True
      | Some(v) -> Unknown

  let project_one box_oct var = Box.I.to_range (Box.get box_oct.box var)
  let project box_oct vars = List.map (fun v -> (v, project_one box_oct v)) vars

  let meet_var box_oct var (l,u) =
    if B.lt u l then raise Bot.Bot_found;
    let itv = Box.I.of_bounds l u in
    { box_oct with box=Box.meet_var var itv box_oct.box }

  let init box_vars octagon_vars initial_constraints reified_octagonal =
    let (constraints, octagon) = Octagon.init octagon_vars initial_constraints in
    (* Printf.printf "Initially: %d constraints \n" (List.length constraints);
    Printf.printf "Initially: %d constraints non-octagonal\n" (List.length (List.filter (fun (octagonal, _) -> not octagonal) constraints));
    Printf.printf "Initially: %d constraints octagonal \n" (List.length (List.filter (fun (octagonal, _) -> octagonal) constraints)); *)
    let box_constraints = List.map snd (List.filter (fun (octagonal, _) -> not octagonal) constraints) in
    let (rotated_constraints, rotated_vars) = rotate octagon_vars box_constraints in
    let box = Box.init (octagon_vars@box_vars@List.map fst rotated_vars) in
    (* We perform a first closure to fix the bound of the domains. *)
    let box = List.fold_left Box.closure box initial_constraints in
    let env_add env (name, key) = Env.add name key env in
    let renv_add renv (name, key) = REnv.add key name renv in
    let box_oct = {
      box_vars=octagon_vars@box_vars;
      box=box;
      octagon=octagon;
      constraints=box_constraints@rotated_constraints;
      reified_octagonal=reified_octagonal;
      env=List.fold_left env_add Env.empty rotated_vars;
      renv=List.fold_left renv_add REnv.empty rotated_vars;
    } in
    let box_oct = {box_oct with box=meet_dbm_in_box box_oct.env box_oct.octagon box_oct.box} in
(*     Printf.printf "In box_oct: %d constraints \n" (List.length box_oct.constraints);
    Printf.printf "In box_oct: %d reified constraints \n" (List.length box_oct.reified_octagonal);
    List.iter (fun (e1,op,e2) -> Format.printf "%a\n" print_bexpr (Cmp (op,e1,e2))) box_oct.constraints; *)
    box_oct
end
 *)