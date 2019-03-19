open Var_store
open Abstract_domain

module type Box_sig =
sig
  type t
  module I: Itv_sig.ITV
  type itv = I.t
  type bound = I.B.t

  val init: Csp.var list -> Csp.bconstraint list -> t
  val get: t -> Csp.var -> itv
  val project_one: t -> Csp.var -> (bound * bound)
  val project: t -> Csp.var list -> (Csp.var * (bound * bound)) list
  val weak_incremental_closure: t -> Csp.bconstraint -> t
  val closure: t -> t
  val incremental_closure: t -> Csp.bconstraint -> t
  val entailment: t -> Csp.bconstraint -> kleene
  val volume: t -> float
  val state_decomposition: t -> kleene
  val print: Format.formatter -> t -> unit
  val split: t -> t list
end

module type Box_functor = functor (B: Bound_sig.BOUND) -> Box_sig with module I.B = B

module Make
  (B: Bound_sig.BOUND)
  (INTERVAL: Itv_sig.Itv_functor)
  (STORE: Var_store_functor)
  (CLOSURE: Hc4.Box_closure_sig)
  (SPLIT: Box_split.Box_split_sig) =
struct
  module Interval = INTERVAL(B)
  module Store = STORE(Interval)
  module Closure = CLOSURE(Store)
  module Split = SPLIT(Store)
  module I = Store.I
  module B = I.B
  type itv = I.t
  type bound = I.B.t
  type t = {
    store: Store.t;
    constraints: Csp.bconstraint list;
  }

  (* Reexported functions from the parametrized modules. *)
  let entailment box = Closure.entailment box.store

  (* This function helps to deal with unreachable state: normally a disentailed constraint triggered a `Bot_found` exception. *)
  let failure_disentailment () =
    failwith "Found a constraint that is disentailed and Bot_found has not been raised."

  let init vars constraints =
    let store = List.fold_left (fun box name -> Store.add name I.top box) Store.empty vars in
    { store = store; constraints = constraints }

  let get box v = Store.find v box.store

  let project_one box v = I.to_range (get box v)
  let project box vars = Store.fold (fun v d l -> (v, I.to_range d)::l) box.store []

  let volume box =
    let range (l,h) =
      if B.equal l h then B.one
      else B.add_up (B.sub_up h l) B.one in
    let size itv = range (I.to_range itv) in
    let vol = B.to_float_up (Store.fold (fun _ x v -> B.mul_up (size x) v) box.store B.one) in
    if classify_float vol = FP_infinite || classify_float vol = FP_nan then
      infinity
    else
      vol

  (* We propagate all the constraints in box.
     The volume is used to detect when the store changed. *)
  let rec propagate vol box =
    let store = List.fold_left Closure.incremental_closure box.store box.constraints in
    let box = { box with store=store } in
    let vol' = volume box in
    if vol <> vol' then
      propagate vol' box
    else
      (vol, box)

  (* We remove the constraints entailed in the store.
     It is useful to avoid propagating entailed constraints. *)
  let remove_entailed_constraints box =
    let is_unknown c =
      match entailment box c with
      | Unknown -> true
      | True -> false
      | False -> failure_disentailment () in
    { box with constraints=List.filter is_unknown box.constraints }

  let closure box =
    let vol = volume box in
    let (vol', box) = propagate vol box in
    if vol <> vol' then
      remove_entailed_constraints box
    else
      box

  let weak_incremental_closure box c = { box with constraints=c::box.constraints }
  let incremental_closure box c = closure (weak_incremental_closure box c)

  (* `closure` and `incremental_closure` automatically remove entailed constraints. *)
  let state_decomposition box =
    if (List.length box.constraints) = 0 then
      True
    else
      Unknown

  let print fmt box =
  begin
    Store.print fmt box.store;
    Format.fprintf fmt "\n";
    List.iter (Format.fprintf fmt "%a\n" Csp.print_bconstraint) box.constraints;
  end

  let split box = List.map (weak_incremental_closure box) (Split.split box.store)
end

module Box_base(SPLIT: Box_split.Box_split_sig) : Box_functor = functor (B: Bound_sig.BOUND) ->
  Make(B)(Itv.Itv)(Var_store.Make)(Hc4.Make)(SPLIT)
