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

module type ValueDistributor = functor (DBM: DBM_sig) ->
sig
  module DBM : DBM_sig
  val distribute: DBM.t -> dbm_interval -> (DBM.bound dbm_constraint) list
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

(* This module factorizes `Width_order` and `Max/Min_LB/UB`. *)
module Dom_order(Fold_interval: Fold_interval_sig)(DBM : DBM_sig) =
struct
  module DBM=DBM
  module B=DBM.B

  let select dbm measure cmp =
    let var =
      Fold_interval.fold (fun a v ->
        let (lb, ub) = DBM.project dbm v in
        if B.equal lb ub then a
        else
          let m = measure (lb, ub) in
          match a with
          | Some (best, _) when cmp m best -> Some (m, v)
          | Some _ -> a
          | None -> Some (m, v)
      ) None (DBM.dimension dbm) in
    match var with
    | Some (_, v) -> Some v
    | None -> None
end


(* This module factorizes `First_fail` and `Anti_first_fail`. *)
module Width_order(Fold_interval: Fold_interval_sig)(DBM : DBM_sig) =
struct
  module DBM=DBM
  module D = Dom_order(Fold_interval)(DBM)
  let select dbm width_cmp =
    let size (lb,ub) = DBM.B.sub_up ub lb in
    D.select dbm size width_cmp
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

module Min_LB(Fold_interval: Fold_interval_sig)(DBM : DBM_sig) =
struct
  module DBM=DBM
  module D = Dom_order(Fold_interval)(DBM)
  let select dbm = D.select dbm fst DBM.B.lt
end

module Min_UB(Fold_interval: Fold_interval_sig)(DBM : DBM_sig) =
struct
  module DBM=DBM
  module D = Dom_order(Fold_interval)(DBM)
  let select dbm = D.select dbm snd DBM.B.lt
end

module Max_LB(Fold_interval: Fold_interval_sig)(DBM : DBM_sig) =
struct
  module DBM=DBM
  module D = Dom_order(Fold_interval)(DBM)
  let select dbm = D.select dbm fst DBM.B.gt
end

module Max_UB(Fold_interval: Fold_interval_sig)(DBM : DBM_sig) =
struct
  module DBM=DBM
  module D = Dom_order(Fold_interval)(DBM)
  let select dbm = D.select dbm snd DBM.B.gt
end

(* An octagon is an intersection of boxes.
   We take the set of the smallest (resp. largest) variables of each box.
   From this set, we take the maximum (resp. minimum) variable.
   It can be instantiated to the Min_max and Max_min strategies. *)
module Max_min(DBM : DBM_sig) =
struct
  module DBM = DBM
  module B = DBM.B
  module R = Bound_rat
  module Itv_view = Interval_view_dbm.Interval_view(R)

  let neutral = R.inf
  let null_itv = {lb={l=(-1);c=(-1)}; ub={l=(-1);c=(-1)}}
  let no_min = (neutral, null_itv)

  let size dbm v =
    let (lb, ub) = DBM.project dbm v in
    if B.equal lb ub then
      R.zero
    else
      let (lb, ub) = Itv_view.dbm_to_itv v (B.to_rat lb, B.to_rat ub) in
      R.sub_up ub lb

  (* This function computes the three first maximal values of the canonical variables. *)
  let max_of_canonical_box dbm =
    Fold_intervals_canonical.fold (fun ((min1,min2,min3) as mini) v ->
      let width = size dbm v in
      if R.equal width R.zero then
        mini
      else
        if (snd min1) == null_itv || R.lt width (fst min1) then ((width,v), min1, min2)
        else if (snd min2) == null_itv || R.lt width (fst min2) then (min1, (width,v), min2)
        else if (snd min3) == null_itv || R.lt width (fst min3) then (min1, min2, (width,v))
        else mini
    ) (no_min,no_min,no_min) (DBM.dimension dbm)

  let max_of v v' = if R.geq (fst v) (fst v') then v else v'
  let min_of v v' = if R.leq (fst v) (fst v') then v else v'

  (* Given a rotated variable, we retrieve the maximum between this variable and the canonical variables. *)
  let select_min (min1, min2, min3) current =
    (* For canonical variables, the lower and upper bounds have the same dimension (they are in the same "sub-square" of the matrix).
       min1, min2 and min3 are canonical variables, thus they operate on a single variable and have only one dimension. *)
    let dim_x v = (snd current).lb.l / 2 in
    let k_x = dim_x current in
    let k_y = (snd current).lb.c / 2 in
    if dim_x min1 <> k_x && dim_x min1 <> k_y then
      min_of min1 current
    else if dim_x min2 <> k_x && dim_x min2 <> k_y then
      min_of min2 current
    else
      min_of min3 current

  (* We compute the maximum values of all the rotated boxes. *)
  let min_of_rotated_boxes dbm canonical_min =
    List.flatten (
      List.map (fun l ->
        List.map (fun c ->
          let var1 = as_interval {l=l*2; c=c*2} in
          let var2 = as_interval {l=l*2; c=c*2+1} in
          let width1 = size dbm var1 in
          let width2 = size dbm var2 in
          let r_vars = List.filter (fun (w,_) -> R.neq w R.zero) [(width1,var1);(width2,var2)] in
          match List.length r_vars with
          | 0 -> select_min canonical_min (neutral, var1)
          | 1 -> select_min canonical_min (List.nth r_vars 0)
          | _ -> min_of (select_min canonical_min (List.nth r_vars 0)) (select_min canonical_min (List.nth r_vars 1))
        ) (Tools.range 0 (l-1))
      ) (Tools.range 1 ((DBM.dimension dbm)-1)))

  let max_of_min canonical_min rotated_min =
    List.fold_left max_of canonical_min rotated_min

  let select dbm =
    let (min1, min2, min3) = max_of_canonical_box dbm in
    if (snd min1) == null_itv then None
    else
      let min_rotated = min_of_rotated_boxes dbm (min1, min2, min3) in
      let min_rotated = List.filter (fun (w,v) -> R.neq w R.zero && v <> null_itv) min_rotated in
      let (width,var) = max_of_min min1 min_rotated in
      Some var
end

(* An octagon is an intersection of boxes.
   We select the smallest variable in the largest plane. *)
module Min_max(DBM : DBM_sig) =
struct
  module DBM = DBM
  module B = DBM.B
  module R = Bound_rat
  module Itv_view = Interval_view_dbm.Interval_view(R)

  let null_itv = {lb={l=(-1);c=(-1)}; ub={l=(-1);c=(-1)}}
  let no_max = (R.zero, null_itv)

  let size dbm v =
    let (lb, ub) = DBM.project dbm v in
    if B.equal lb ub then
      R.zero
    else
      let (lb, ub) = Itv_view.dbm_to_itv v (B.to_rat lb, B.to_rat ub) in
      R.sub_up ub lb

  (* This function computes the three first maximal values of the canonical variables. *)
  let max_of_canonical_box dbm =
    Fold_intervals_canonical.fold (fun ((max1,max2,max3) as maxi) v ->
      let width = size dbm v in
      if R.equal width R.zero then
        maxi
      else
        if (snd max1) == null_itv || R.gt width (fst max1) then ((width,v), max1, max2)
        else if (snd max2) == null_itv || R.gt width (fst max2) then (max1, (width,v), max2)
        else if (snd max3) == null_itv || R.gt width (fst max3) then (max1, max2, (width,v))
        else maxi
    ) (no_max,no_max,no_max) (DBM.dimension dbm)

  let max_of v v' = if R.geq (fst v) (fst v') then v else v'
  let min_of v v' = if R.leq (fst v) (fst v') then v else v'

  (* Given a rotated variable, we retrieve the maximum between this variable and the canonical variables. *)
  let select_max (max1, max2, max3) current =
    (* For canonical variables, the lower and upper bounds have the same dimension (they are in the same "sub-square" of the matrix).
       max1, max2 and max3 are canonical variables, thus they operate on a single variable and have only one dimension. *)
    let dim_x v = (snd current).lb.l / 2 in
    let k_x = dim_x current in
    let k_y = (snd current).lb.c / 2 in
    if dim_x max1 <> k_x && dim_x max1 <> k_y then
      max_of max1 current
    else if dim_x max2 <> k_x && dim_x max2 <> k_y then
      max_of max2 current
    else
      max_of max3 current

  (* We compute the maximum values of all the rotated boxes. *)
  let max_of_rotated_boxes dbm canonical_max =
    List.flatten (
      List.map (fun l ->
        List.map (fun c ->
          let var1 = as_interval {l=l*2; c=c*2} in
          let var2 = as_interval {l=l*2; c=c*2+1} in
          let width1 = size dbm var1 in
          let width2 = size dbm var2 in
          let r_vars = List.filter (fun (w,_) -> R.neq w R.zero) [(width1,var1);(width2,var2)] in
          match List.length r_vars with
          | 0 -> select_max canonical_max (R.zero, var1)
          | 1 -> select_max canonical_max (List.nth r_vars 0)
          | _ -> max_of (select_max canonical_max (List.nth r_vars 0)) (select_max canonical_max (List.nth r_vars 1))
        ) (Tools.range 0 (l-1))
      ) (Tools.range 1 ((DBM.dimension dbm)-1)))

  let min_of_max canonical_max rotated_max =
    List.fold_left min_of canonical_max rotated_max

  let select dbm =
    let (max1, max2, max3) = max_of_canonical_box dbm in
    if (snd max1) == null_itv then None
    else
      let max_rotated = max_of_rotated_boxes dbm (max1, max2, max3) in
      let max_rotated = List.filter (fun (w,v) -> R.neq w R.zero && v <> null_itv) max_rotated in
      let (width,var) = min_of_max max1 max_rotated in
      Some var
end

(** Should be combined with `BisectLB` *)
module Middle (DBM: DBM_sig) =
struct
  module DBM = DBM
  module B = DBM.B
  let select dbm v =
    let (lb, ub) = DBM.project dbm v in
    B.add_up lb (B.div_down (B.sub_up ub lb) B.two)
end

(** Should be combined with `BisectLB` *)
module Lower_bound (DBM: DBM_sig) =
struct
  module DBM = DBM
  let select dbm itv = fst (DBM.project dbm itv)
end

(** Should be combined with `BisectUB` *)
module Upper_bound (DBM: DBM_sig) =
struct
  module DBM = DBM
  let select dbm itv = snd (DBM.project dbm itv)
end

(** x <= v \/ x > v *)
module BisectLB (DBM: DBM_sig) =
struct
  module DBM=DBM
  module B=DBM.B
  let distribute itv value =
    [{v=itv.ub; d=value};
     {v=itv.lb; d=B.neg (B.succ value)}]
end

(** x < v \/ x >= v *)
module BisectUB (DBM: DBM_sig) =
struct
  module DBM=DBM
  module B=DBM.B
  let distribute itv value =
    [{v=itv.ub; d=B.prec value};
     {v=itv.lb; d=B.neg value}]
end

module Distribute_value(VALUE: Value_order)(DISTRIBUTOR: Distributor)(DBM: DBM_sig) =
struct
  module DBM=DBM
  module Value = VALUE(DBM)
  module Distributor = DISTRIBUTOR(DBM)
  let distribute dbm itv = Distributor.distribute itv (Value.select dbm itv)
end

module Bisect_middle = Distribute_value(Middle)(BisectLB)
module Assign_LB = Distribute_value(Lower_bound)(BisectLB)
module Assign_UB = Distribute_value(Upper_bound)(BisectUB)

module Right_to_left(DISTRIBUTOR: ValueDistributor)(DBM: DBM_sig) =
struct
  module DBM=DBM
  module D=DISTRIBUTOR(DBM)
  let distribute itv value = List.rev (D.distribute itv value)
end

module Make
  (VARIABLE: Variable_order)
  (VALUE_DISTRIBUTOR: ValueDistributor)
  (DBM: DBM_sig) =
struct
  module DBM=DBM
  module Variable = VARIABLE(DBM)
  module Value_distributor = VALUE_DISTRIBUTOR(DBM)

  let split dbm =
    match Variable.select dbm with
    | None -> []
    | Some itv -> Value_distributor.distribute dbm itv
end

module First_fail_bisect = Make(First_fail(Fold_intervals))(Bisect_middle)
module First_fail_LB_canonical = Make(First_fail(Fold_intervals_canonical))(Assign_LB)
module Min_max_LB = Make(Min_max)(Assign_LB)
module Max_min_LB = Make(Max_min)(Assign_LB)
module Max_min_UB = Make(Max_min)(Assign_UB)
module Max_min_Bisect = Make(Max_min)(Bisect_middle)
module Max_min_Bisect_reverse = Make(Max_min)(Right_to_left(Bisect_middle))
module Anti_first_fail_LB_canonical = Make(Anti_first_fail(Fold_intervals_canonical))(Assign_LB)
module Anti_first_fail_UB_canonical = Make(Anti_first_fail(Fold_intervals_canonical))(Assign_UB)
module Anti_first_fail_LB = Make(Anti_first_fail(Fold_intervals))(Assign_LB)
module Anti_first_fail_UB = Make(Anti_first_fail(Fold_intervals))(Assign_UB)
(* module MSLF = Make(Min_LB(Fold_intervals_canonical))(Bisect_middle)
module MSLF_rotated = Make(Min_LB(Fold_intervals_rotated))(Bisect_middle)
module MSLF_all = Make(Min_LB(Fold_intervals))(Bisect_middle)
 *)
module MSLF = Make(Max_UB(Fold_intervals_canonical))(Right_to_left(Bisect_middle))
module MSLF_rotated = Make(Max_UB(Fold_intervals_rotated))(Right_to_left(Bisect_middle))
module MSLF_all = Make(Max_UB(Fold_intervals))(Right_to_left(Bisect_middle))

