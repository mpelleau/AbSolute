open Dbm

module type Variable_order = functor (DBM : DBM_sig) ->
sig
  module DBM : DBM_sig
  val select: DBM.t -> dbm_interval option
end with module DBM=DBM

module type Value_order = functor (DBM: DBM_sig) ->
sig
  module DBM : DBM_sig
  val select: DBM.t -> dbm_interval -> DBM.bound
end with module DBM=DBM

module type Distributor = functor (DBM: DBM_sig) ->
sig
  module DBM : DBM_sig
  val distribute: dbm_interval -> DBM.bound -> (DBM.bound dbm_constraint) list
end with module DBM=DBM

module type Octagon_split_sig = functor (DBM : DBM_sig) ->
sig
  module DBM : DBM_sig
  val split: DBM.t -> (DBM.bound dbm_constraint) list
end with module DBM=DBM

module Input_order(Fold_interval: Fold_interval_sig)(DBM : DBM_sig) =
struct
  module DBM=DBM
  module B=DBM.B

  exception Found_var of dbm_interval
  let select dbm =
    try
      Fold_interval.fold (fun _ v ->
        let (lb, ub) = DBM.project dbm v in
        if B.neq lb ub then raise (Found_var v)
      ) () (DBM.dimension dbm);
      None
    with Found_var v -> Some v
end

(* This module factorizes `First_fail` and `Anti_first_fail`. *)
module Width_order(Fold_interval: Fold_interval_sig)(DBM : DBM_sig) =
struct
  module DBM=DBM
  module B=DBM.B

  let select dbm width_cmp =
    let size (lb,ub) = B.sub_up ub lb in
    let var =
      Fold_interval.fold (fun a v ->
        let (lb, ub) = DBM.project dbm v in
        if B.equal lb ub then a
        else
          let width = size (lb, ub) in
          match a with
          | Some (best, _) when width_cmp width best -> Some (width, v)
          | Some _ -> a
          | None -> Some (width, v)
      ) None (DBM.dimension dbm) in
    match var with
    | Some (_, v) -> Some v
    | None -> None
end

module First_fail(Fold_interval: Fold_interval_sig)(DBM : DBM_sig) =
struct
  module DBM = DBM
  module W = Width_order(Fold_interval)(DBM)
  let select dbm = W.select dbm DBM.B.lt
end

module Anti_first_fail(Fold_interval: Fold_interval_sig)(DBM : DBM_sig) =
struct
  module DBM = DBM
  module W = Width_order(Fold_interval)(DBM)
  let select dbm = W.select dbm DBM.B.gt
end

(* An octagon is an intersection of boxes.
   We select the smallest variable in the largest box. *)
module Min_max(DBM : DBM_sig) =
struct
  module DBM = DBM
  module B = DBM.B

  let size dbm v =
    let (lb, ub) = DBM.project dbm v in
    B.sub_up ub lb

  let select_max (max1, max2, max3) current =
    let max_of v v' = if B.geq (fst v) (fst v') then v else v' in
    let dim_x v = (snd current).lb.l / 2 in
    (* Note that the lower and upper bounds have the same dimension (they are in the same "sub-square" of the matrix.
       max1, max2 and max3 are canonical variables, thus they operate on a single variable and have only one dimension. *)
    let k_x = dim_x current in
    let k_y = (snd current).lb.c / 2 in
    if dim_x max1 <> k_x && dim_x max1 <> k_y then
      max_of max1 current
    else if dim_x max2 <> k_x && dim_x max2 <> k_y then
      max_of max2 current
    else
      max_of max3 current

  let select dbm =
    let null_itv = {lb={l=0;c=0}; ub={l=0;c=0}} in
    let no_max = (B.zero, null_itv) in
    let (max1, max2, max3) =
      Fold_intervals_canonical.fold (fun (max1,max2,max3) v ->
        let width = size dbm v in
        if B.gt width (fst max1) then ((width,v), max2, max3)
        else if B.gt width (fst max2) then (max1, (width,v), max3)
        else if B.gt width (fst max3) then (max1, max2, (width,v))
        else (max1,max2,max3)
      ) (no_max,no_max,no_max) (DBM.dimension dbm) in
    if max1 = no_max then None
    else Some (snd (
      Fold_intervals_rotated.fold (fun min v ->
        let width = size dbm v in
        let max = select_max (max1, max2, max3) (width, v) in
        if B.lt (fst max) (fst min) then max else min
      ) (B.inf, null_itv) (DBM.dimension dbm)))
end

module Middle (DBM: DBM_sig) =
struct
  module DBM = DBM
  module B = DBM.B
  let select dbm v =
    let (lb, ub) = DBM.project dbm v in
    B.div_down (B.add_up lb ub) B.two
end

module Lower_bound (DBM: DBM_sig) =
struct
  module DBM = DBM
  let select dbm v = fst (DBM.project dbm v)
end

module Upper_bound (DBM: DBM_sig) =
struct
  module DBM = DBM
  let select dbm v = snd (DBM.project dbm v)
end

module Bisect (DBM: DBM_sig) =
struct
  module DBM=DBM
  module B=DBM.B
  let distribute itv value =
    [{v=itv.ub; d=B.prec value};
     {v=itv.lb; d=B.neg value}]
end

module Bisect_reverse (DBM: DBM_sig) =
struct
  module DBM=DBM
  module B=DBM.B
  let distribute itv value =
    [{v=itv.lb; d=B.neg value};
     {v=itv.ub; d=B.prec value}]
end

module Make
  (VARIABLE: Variable_order)
  (VALUE: Value_order)
  (DISTRIBUTOR: Distributor)
  (DBM: DBM_sig) =
struct
  module DBM=DBM
  module Variable = VARIABLE(DBM)
  module Value = VALUE(DBM)
  module Distributor = DISTRIBUTOR(DBM)

  let split dbm =
    match Variable.select dbm with
    | None -> []
    | Some itv -> Distributor.distribute itv (Value.select dbm itv)
end

module First_fail_bisect = Make(First_fail(Fold_intervals))(Middle)(Bisect)
