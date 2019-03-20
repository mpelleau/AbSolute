open Abstract_domain
open Dbm

module type Octagon_sig =
sig
  module DBM : DBM_sig
  module B = DBM.B
  type bound = B.t
  type t
  val init: int -> t
  val copy: t -> t
  val entailment: t -> bound dbm_constraint -> kleene
  val closure: t -> t
  val incremental_closure: t -> bound dbm_constraint -> t
  val weak_incremental_closure: t -> bound dbm_constraint -> t
  val unwrap: t -> DBM.t
  val split: t -> t list
  val state_decomposition: t -> kleene
  val project: t -> dbm_interval -> (bound * bound)
  val volume: t -> float
  val print: Format.formatter -> t -> unit
end

module Make
  (Closure: Closure.Closure_sig)
  (SPLIT: Octagon_split.Octagon_split_sig) =
struct
  module DBM = Closure.DBM
  module Split = SPLIT(DBM)
  module B = DBM.B
  type bound = B.t

  module Itv_view = Interval_view_dbm.Interval_view(B)

  type t = {
    dbm: DBM.t;
    (* These constraints must be coherent (see `Dbm.ml`). *)
    constraints: (bound dbm_constraint) list;
  }

  let init dimension = {dbm=DBM.init dimension; constraints=[]}
  let copy octagon = { octagon with dbm=DBM.copy octagon.dbm; }
  let print fmt octagon = DBM.print fmt octagon.dbm

  let entailment octagon oc =
    (* I. Retrieve the current lower and upper bound of `oc`. *)
    let itv = as_interval oc.v in
    let (lb, ub) = DBM.project octagon.dbm itv in
    let is_lb = is_lower_bound oc.v in
    let bound = if is_lb then B.neg oc.d else oc.d in
    (* II. We decide entailment by comparing the current bounds and `oc.c`. *)
    if (is_lb && B.leq bound lb) ||
       (not is_lb && B.geq bound ub) then True
    else if (is_lb && B.gt bound ub) ||
            (not is_lb && B.lt bound lb) then False
    else Unknown

  let closure octagon =
    (if (List.length octagon.constraints) >= (DBM.dimension octagon.dbm) then
      (List.iter (DBM.set octagon.dbm) octagon.constraints;
      Closure.closure octagon.dbm)
    else
      List.iter (Closure.incremental_closure octagon.dbm) octagon.constraints;
    {octagon with constraints=[]})

  let weak_incremental_closure octagon oc =
    match entailment octagon oc with
    | True -> octagon
    | False -> raise Bot.Bot_found
    | Unknown -> { octagon with constraints=oc::octagon.constraints }

  let incremental_closure octagon oc =
    let octagon' = (weak_incremental_closure octagon oc) in
    if octagon <> octagon' then closure octagon' else octagon

  let unwrap octagon = octagon.dbm

  let split octagon = List.mapi (fun i branch ->
    weak_incremental_closure (if i > 0 then copy octagon else octagon) branch
  ) (Split.split octagon.dbm)

  let state_decomposition octagon =
    if (List.length octagon.constraints) = 0 then
      True
    else
      Unknown

  let project octagon itv = Itv_view.dbm_to_itv itv (DBM.project octagon.dbm itv)

  (* Get the value of the lower bound and the volume between the lower and upper bound. *)
  let volume_of octagon itv =
    let (lb, ub) = project octagon itv in
    B.add_up B.one (B.sub_up ub lb)

  let volume octagon = B.to_float_up (Fold_intervals_canonical.fold (fun a itv ->
      B.mul_up a (volume_of octagon itv)
    ) B.one (DBM.dimension octagon.dbm))
end

module OctagonZ(SPLIT: Octagon_split.Octagon_split_sig) = Make(Closure.ClosureZ)(SPLIT)
module OctagonQ(SPLIT: Octagon_split.Octagon_split_sig) = Make(Closure.ClosureQ)(SPLIT)
module OctagonF(SPLIT: Octagon_split.Octagon_split_sig) = Make(Closure.ClosureF)(SPLIT)
