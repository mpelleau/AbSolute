open Csp
open Abstract_domain
open Box_dom

type box_reified_constraint = var * bconstraint list

module type Box_reified_sig =
sig
  type t
  module I: Itv_sig.ITV
  module B = I.B
  type bound = B.t
  type itv = I.t

  val init: var list -> bconstraint list -> box_reified_constraint list -> t
  val get: t -> Csp.var -> itv
  val project_one: t -> var -> (bound * bound)
  val project: t -> var list -> (var * (bound * bound)) list
  val weak_incremental_closure: t -> Csp.bconstraint -> t
  val closure: t -> t
  val incremental_closure: t -> Csp.bconstraint -> t
  val entailment: t -> Csp.bconstraint -> kleene
  val volume: t -> float
  val state_decomposition: t -> kleene
  val print: Format.formatter -> t -> unit
  val split: t -> t list
end

module Make(Box: Box_sig) =
struct
  module I = Box.I
  module B = I.B
  type bound = B.t
  type itv = I.t
  type t = {
    inner: Box.t;
    reified_constraints: box_reified_constraint list;
  }

  let init vars constraints reified_constraints =
    { inner=Box.init vars constraints; reified_constraints=reified_constraints }

  (* The following functions just forward the call to `Box`. *)
  let entailment box = Box.entailment box.inner
  let get box v = Box.get box.inner v
  let project_one box v = Box.project_one box.inner v
  let project box vars = Box.project box.inner vars
  let volume box = Box.volume box.inner
  let weak_incremental_closure box c = { box with inner=Box.weak_incremental_closure box.inner c}

  let propagate_negation_conjunction box (b, conjunction) =
    match and_reified (List.map (entailment box) conjunction) with
    | False,_ -> box
    | True,_ -> raise Bot.Bot_found
    | Unknown, Some(u) ->
        let (e1,op,e2) = (List.nth conjunction u) in
        weak_incremental_closure box (e1, Csp.neg op, e2)
    | Unknown, None ->
        { box with reified_constraints=(b, conjunction)::box.reified_constraints}

  (* Propagate the reified constraints.
     Entailed reified constraints are removed from `box`. *)
  let propagate_reified box (b, conjunction) =
    let itv = Box.get box.inner b in
    if Box.I.is_singleton itv then
      let (value,_) = Box.I.to_range itv in
      if B.equal B.one value then
        List.fold_left weak_incremental_closure box conjunction
      else if B.equal B.zero value then
        propagate_negation_conjunction box (b, conjunction)
      else failwith "Reified boolean should be equal to 0 or 1."
    else
      match fst (and_reified (List.map (entailment box) conjunction)) with
      | False -> weak_incremental_closure box (Var b, EQ, constant_zero)
      | True -> weak_incremental_closure box (Var b, EQ, constant_one)
      | Unknown -> { box with reified_constraints=(b, conjunction)::box.reified_constraints }

  let propagate_all_reified box =
    List.fold_left propagate_reified { box with reified_constraints=[] } box.reified_constraints

  let rec propagate box =
    let box = { box with inner=Box.closure box.inner } in
    let count_reified = List.length box.reified_constraints in
    let box = propagate_all_reified box in
    let count_reified' = List.length box.reified_constraints in
    if count_reified <> count_reified' then
      propagate box
    else
      box

  let closure box = propagate box

  let incremental_closure box c = closure (weak_incremental_closure box c)

  let state_decomposition box =
    let state = if (List.length box.reified_constraints) = 0 then True else Unknown in
    and_kleene state (Box.state_decomposition box.inner)

  let print_reified_constraint fmt (b, conjunction) =
  begin
    Format.fprintf fmt "%s <=> " b;
    List.iter (Format.fprintf fmt "%a /\\" Csp.print_bconstraint) conjunction;
    Format.fprintf fmt "\n";
  end

  let print fmt box =
  begin
    Box.print fmt box.inner;
    Format.fprintf fmt "\n";
    List.iter (print_reified_constraint fmt) box.reified_constraints;
  end

  let split box = List.map (fun branch -> { box with inner=branch}) (Box.split box.inner)
end

module BoxReifiedZ(SPLIT: Box_split.Box_split_sig) = Make(Box_base(SPLIT)(Bound_int))
module BoxReifiedQ(SPLIT: Box_split.Box_split_sig) = Make(Box_base(SPLIT)(Bound_rat))
module BoxReifiedF(SPLIT: Box_split.Box_split_sig) = Make(Box_base(SPLIT)(Bound_float))