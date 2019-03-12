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

module Make
  (B: Bound_sig.BOUND)
  (I: Itv_sig.ITV with type bound=B.t)
  (Store: Var_store_sig with type cell=I.t)
  (Closure: Box_closure_sig with module Store=Store) =
struct
  module B = B
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

  exception Found_var of Csp.var * itv

  let input_order_var box =
    try
      Store.iter (fun v d -> if not (I.is_singleton d) then raise (Found_var (v,d))) box.store;
      None
    with Found_var (v,d) -> Some (v,d)

  let select_domain box width_cmp =
    let size (l,h) = B.sub_up h l in
    let var = Store.fold (fun v d a ->
      if I.is_singleton d then a
      else
        let width = size (I.to_range d) in
        match a with
        | Some (best,v',d') when width_cmp width best -> Some (width,v,d)
        | Some _ -> a
        | None -> Some (width,v,d)) box.store None in
    match var with
    | Some (_, v,d) -> Some (v,d)
    | None -> None

  let smallest_domain box = select_domain box B.lt
  let largest_domain box = select_domain box B.gt

  let assign box var value =
    let open Csp in
    let left_box = weak_incremental_closure box (Var var, EQ, value) in
    let right_box = weak_incremental_closure box (Var var, NEQ, value) in
    [left_box ; right_box]

  let bisect box var value =
    let open Csp in
    let left_box = weak_incremental_closure box (Var var, LEQ, value) in
    let right_box = weak_incremental_closure box (Var var, GT, value) in
    [left_box ; right_box]

  let middle itv =
    let open Csp in
    let (l,u) = I.to_range itv in
    Cst (B.to_rat (B.div_down (B.add_up l u) B.two), Int)

  let on_bound itv select = Csp.(Cst (B.to_rat (select (I.to_range itv)), Int))
  let on_lb itv = on_bound itv fst
  let on_ub itv = on_bound itv snd

  let split box =
    match largest_domain box with
    | None -> []
    | Some (v, itv) -> assign box v (on_ub itv)
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
