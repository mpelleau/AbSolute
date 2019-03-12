open Var_store
open Abstract_domain
open Hc4

module type Box_sig =
sig
  type t
  type bound
  module I: Itv_sig.ITV with type bound = bound
  type itv = I.t

  val init: Csp.var list -> Csp.bconstraint list -> t
  val get: t -> Csp.var -> itv
  val project: (Csp.var -> bool) -> t -> t
  val weak_incremental_closure: t -> Csp.bconstraint -> t
  val closure: t -> t
  val incremental_closure: t -> Csp.bconstraint -> t
  val entailment: t -> Csp.bconstraint -> kleene
  val volume: t -> float
  val state_decomposition: t -> kleene
  val print: Format.formatter -> t -> unit
end

module Make
  (B: Bound_sig.BOUND)
  (I: Itv_sig.ITV with type bound=B.t)
  (Store: Var_store_sig with type cell=I.t)
  (Closure: Box_closure_sig with module Store=Store) =
struct
  module I = I
  type itv = I.t
  type bound = B.t
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
  let project f box = { box with store=Store.filter (fun v _ -> f v) box.store }

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
    let _ = (Printf.printf "[propagate] constraints = %d\n" (List.length box.constraints); flush_all ();) in
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

  let print_constraint fmt (e1,op,e2) =
    let open Csp in
    Format.fprintf fmt "%a\n" print_bexpr (Cmp (op,e1,e2))

  let print fmt box =
  begin
    Store.print fmt box.store;
    Format.fprintf fmt "\n";
    List.iter (print_constraint fmt) box.constraints;
  end
end

module ItvZ = Trigo.Make(Itv.ItvI)
module ItvQ = Trigo.Make(Itv.ItvQ)
module ItvF = Trigo.Make(Itv.ItvF)

module StoreZ = Var_store.Make(ItvZ)
module StoreQ = Var_store.Make(ItvQ)
module StoreF = Var_store.Make(ItvF)

module BoxZ = Make
  (Bound_int)
  (ItvZ)
  (StoreZ)
  (Hc4.Make(ItvZ)(StoreZ))

module BoxQ = Make
  (Bound_rat)
  (ItvQ)
  (StoreQ)
  (Hc4.Make(ItvQ)(StoreQ))

module BoxF = Make
  (Bound_float)
  (ItvF)
  (StoreF)
  (Hc4.Make(ItvF)(StoreF))
