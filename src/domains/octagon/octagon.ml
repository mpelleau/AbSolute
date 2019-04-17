open Abstract_domain
open Dbm

module type Octagon_sig =
sig
  module DBM : DBM_sig
  module B = DBM.B
  type bound = B.t
  type t
  val init: int -> t
  val copy: t -> int -> t list
  val entailment: t -> bound dbm_constraint -> kleene
  val strong_entailment: t -> bound dbm_constraint -> kleene
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
  let copy octagon n = List.map (fun dbm -> { octagon with dbm=dbm; }) (DBM.copy_n octagon.dbm n)
  let print fmt octagon = DBM.print fmt octagon.dbm

  let entailment octagon oc =
    let current = DBM.get octagon.dbm oc.v in
    if B.geq oc.d current then True
    (* If the addition of the bounds is less than zero then the two sides of the octagon are reversed. *)
    else if B.lt (B.add_up oc.d (DBM.get octagon.dbm (inv oc.v))) B.zero then False
    else Unknown

  let strong_entailment octagon oc =
    match entailment octagon oc with
    | Unknown ->
      begin
        let dbm' = DBM.copy octagon.dbm in
        try
          let _ = Closure.incremental_closure dbm' oc in
          Unknown
        with Bot.Bot_found -> False
      end
    | r -> r

  let closure octagon =
    let dbm =
      if (List.length octagon.constraints) >= (DBM.dimension octagon.dbm) then
        List.fold_left DBM.set octagon.dbm octagon.constraints
        |> Closure.closure
      else
        List.fold_left Closure.incremental_closure octagon.dbm octagon.constraints in
    {dbm=dbm; constraints=[]}

  let weak_incremental_closure octagon oc =
    match entailment octagon oc with
    | True -> octagon
    | False -> raise Bot.Bot_found
    | Unknown -> { octagon with constraints=oc::octagon.constraints }

  let incremental_closure octagon oc =
    let octagon' = (weak_incremental_closure octagon oc) in
    if (List.length octagon.constraints) <> (List.length octagon'.constraints) then
      closure octagon'
    else
      octagon

  let unwrap octagon = octagon.dbm

  let split octagon =
    let branches = Split.split octagon.dbm in
    let octagons = copy octagon (List.length branches) in
    List.map2 weak_incremental_closure octagons branches

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

module OctagonZ(SPLIT: Octagon_split.Octagon_split_sig) = Make(Closure.ClosureHoistZ)(SPLIT)
module OctagonQ(SPLIT: Octagon_split.Octagon_split_sig) = Make(Closure.ClosureQ)(SPLIT)
module OctagonF(SPLIT: Octagon_split.Octagon_split_sig) = Make(Closure.ClosureF)(SPLIT)
