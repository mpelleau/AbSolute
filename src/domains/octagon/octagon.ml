open Csp
open Interval_view_dbm
open Octagonal_rewriting
open Abstract_domain

module type Octagon_sig =
sig
  type t
  type bound
  module Rewriter: Octagonal_rewriting.Rewriter_sig
  val init: var list -> bconstraint list -> ((bool * bconstraint) list * t)
  val empty: t
  val copy: t -> t
  val extend_one: t -> var -> t
  val meet_constraint: t -> bconstraint -> bool
  val fold_vars: (var -> dbm_key -> 'a -> 'a) -> 'a -> t -> 'a
  val iter_vars: (var -> dbm_key -> unit) -> t -> unit
  val set_lb: t -> dbm_key -> bound -> unit
  val set_ub: t -> dbm_key -> bound -> unit
  val lb: t -> dbm_key -> bound
  val ub: t -> dbm_key -> bound
  val closure: t -> unit
  val incremental_closure: t -> octagonal_constraint -> unit
  val dbm_as_list: t -> bound list
  val entailment: t -> octagonal_constraint -> kleene
  val split: t -> t list
  val state_decomposition: t -> kleene
  val project_one: t -> var -> (bound * bound)
  val volume: t -> float
end

module Make
  (B: Bound_sig.BOUND)
  (IntervalView: IntervalViewDBM with type bound=B.t)
  (Closure: Closure.Closure_sig with module DBM = Dbm.Make(B))
  (Rewriter: Octagonal_rewriting.Rewriter_sig) =
struct
  module DBM = Closure.DBM
  module Rewriter = Rewriter
  include IntervalView
  include Rewriter

  module Env = Tools.VarMap
  module REnv = Mapext.Make(struct
    type t=dbm_key
    let compare = compare end)

  (* We keep a bijection between AbSolute variable names (string) and the DBM key.
     Invariant: For all key, we have: `dbm_key = REnv.find (Env.find env dbm_key) renv`. *)
  type t = {
    dbm: DBM.t;
    (* maps each variable name to its `dbm_key` in the dbm. *)
    env : dbm_key Env.t;
    (* reversed mapping of `env`. *)
    renv : string REnv.t;
  }

  let index_of octagon (sign, v) =
    let (d,_) = Env.find v octagon.env in
    match sign with
    | Positive -> d*2+1
    | Negative -> d*2

  let update octagon oc =
    let index_of = index_of octagon in
    DBM.set octagon.dbm (index_of oc.x, index_of oc.y) (B.of_rat_up oc.c)

  (** Reexported functions from the parametrized modules. *)
  let closure octagon = Closure.closure octagon.dbm
    (* I. We empty the set of octagonal constraints by performing incremental closure.
          Normally, only the constraint from the splitting strategy is in this set. *)
    (* List.iter (incremental_closure octagon) octagon.octagonal_constraints; *)

  let incremental_closure octagon oc =
(*     begin update octagon oc; closure octagon end *)
    Closure.incremental_closure octagon.dbm ((index_of octagon oc.x, index_of octagon oc.y), (B.of_rat_up oc.c))

  let is_consistent octagon = Closure.is_consistent octagon.dbm

  let set_lb o k v = DBM.set o.dbm (lb_pos k) (lb_to_dbm k v)
  let set_ub o k v = DBM.set o.dbm (ub_pos k) (ub_to_dbm k v)
  let lb o k = dbm_to_lb k (DBM.get o.dbm (lb_pos k))
  let ub o k = dbm_to_ub k (DBM.get o.dbm (ub_pos k))

  let empty = {
    dbm=DBM.empty;
    env=Env.empty;
    renv=REnv.empty;
  }

  let copy octagon = { octagon with dbm=DBM.copy octagon.dbm; }

  let fold_vars f a octagon = Env.fold f octagon.env a
  let iter_vars f octagon = Env.iter f octagon.env

  let extend_one octagon var =
    let key = (DBM.dimension octagon.dbm, cplane) in
    {
      dbm=DBM.extend_one octagon.dbm;
      env=Env.add var key octagon.env;
      renv=REnv.add key var octagon.renv;
    }

  let meet_constraint octagon c =
    let iter_oct = List.iter (incremental_closure octagon) in
    match rewrite c with
    | [] ->
        (match relax c with
        | [] -> false
        | cons -> (iter_oct cons; false))
    | cons -> (iter_oct cons; true)

  let init vars constraints =
    let octagon = List.fold_left extend_one empty vars in
    let add_cons c =
      if is_defined_over vars c then
        (meet_constraint octagon c, c)
      else
        (false, c) in
    (List.map add_cons constraints, octagon)

  let dbm_as_list octagon = DBM.to_list octagon.dbm

  let dim_of octagon (s, var) = fst (Env.find var octagon.env)

  let dimension_of octagon oc =
    let (sx, x) = oc.x in
    let (sy, y) = oc.y in
    let (is_lb, var) = match (sx, sy) with
      | Positive, Positive -> (false, oc.x)
      | Negative, Negative -> (true, oc.x)
      | Positive, Negative -> (false, oc.y)
      | Negative, Positive -> (true, oc.y) in
    (is_lb, dim_of octagon var)

  let entailment octagon oc =
    let index_of v = index_of octagon v in
    let (is_lb, dim) = dimension_of octagon oc in
    let bound_1 = DBM.get octagon.dbm (index_of oc.x, index_of oc.y) in
    let bound_2 = DBM.get octagon.dbm (index_of (rev oc.x), index_of (rev oc.y)) in
    let (lb, ub) = if is_lb then (B.neg bound_1, bound_2) else (B.neg bound_2, bound_1) in
    let bound = if is_lb then B.neg (B.of_rat_up oc.c) else B.of_rat_up oc.c in

      (* Format.printf "%a" DBM.print octagon.dbm; *)
      (* Printf.printf "Disentailed %s [%s,%s] with %s\n" (octagonal_to_string oc) (B.to_string lb) (B.to_string ub) (B.to_string bound); *)
    if (is_lb && B.leq bound lb) ||
       (not is_lb && B.geq bound ub) then True
    else if (is_lb && B.gt bound ub) ||
            (not is_lb && B.lt bound lb) then False
    else Unknown


  let fold_intervals f accu octagon =
    List.fold_left (fun accu l ->
      List.fold_left (fun accu c ->
        let l = l*2 in
        let c = c*2 in
        let accu = f accu ((l,c+1),(l+1,c)) in
        if l <> c then (* if rotated there are two intervals. *)
          f accu ((l,c),(l+1,c+1))
        else
          accu
      ) accu (Tools.range 0 l)
    ) accu (Tools.range 0 ((DBM.dimension octagon.dbm)-1))

(*     let (width, coords) = fold_intervals (fun (smallest,coord) (lb_coord,ub_coord) ->
      let lb = DBM.get' octagon.dbm lb_coord in
      let ub = DBM.get' octagon.dbm ub_coord in
      let width = B.sub_up ub lb in
      if B.gt width B.one && B.lt width smallest then
        (width, (lb_coord, ub_coord))
      else
        (smallest, coord)
    ) (B.inf, ((0,1),(1,0))) octagon *)

  let fold_intervals_canonical f accu octagon =
    List.fold_left (fun accu k ->
      let k = k*2 in
      f accu ((k,k+1),(k+1,k)))
    accu (Tools.range 0 ((DBM.dimension octagon.dbm)-1))

  exception FoundVar of B.t * B.t * Dbm.coord2D * Dbm.coord2D

  (* Get the value of the lower bound and the volume between the lower and upper bound. *)
  let volume_of octagon (lb_coord, ub_coord) =
    let lb = B.neg (DBM.get' octagon.dbm lb_coord) in
    let ub = DBM.get' octagon.dbm ub_coord in
    let width = B.add_up B.one (B.sub_up ub lb) in
    (lb, width)

  let input_order octagon =
    (* I. select the first non-instantiated variable. *)
    try
      let _ = fold_intervals_canonical (fun a (lb_coord,ub_coord) ->
        let (lb, width) = volume_of octagon (lb_coord, ub_coord) in
        if B.gt width B.one then
          raise (FoundVar(width, lb, lb_coord, ub_coord))
        else
          a
      ) B.inf octagon in
      None
    (* II. assign to the smallest value. *)
    with FoundVar (width, lb, lb_coord, ub_coord) ->
      Some (width, lb, lb_coord, ub_coord)

  let create_branch octagon dbm_constraint =
    try
      Closure.incremental_closure octagon.dbm dbm_constraint;
      [octagon]
    with Bot.Bot_found -> []

  let split octagon =
    match input_order octagon with
    | None -> []
    | Some (width, lb, lb_coord, ub_coord) -> begin
        let left = octagon in
        let right = copy octagon in
        let mid = B.add_up lb (B.div_down width B.two) in
        (create_branch left (ub_coord, mid))@
        (create_branch right (lb_coord, B.neg (B.add_down mid B.one)))
      end

  let state_decomposition octagon =
    match input_order octagon with
    | None -> True
    | Some _ -> Unknown

  let project_one octagon var =
    let key = Env.find var octagon.env in
    (lb octagon key, ub octagon key)

  let volume octagon = B.to_float_up (fold_intervals_canonical (fun a coords ->
    B.mul_up a (snd (volume_of octagon coords))) B.one octagon)
end

module DBM_Z = Dbm.Make(Bound_int)
module OctagonZ = Make
  (Bound_int)
  (IntegerIntervalDBM)
  (Closure.ClosureZ(DBM_Z))
  (Octagonal_rewriting.RewriterZ)

module DBM_Q = Dbm.Make(Bound_rat)
module OctagonQ = Make
  (Bound_rat)
  (RationalIntervalDBM)
  (Closure.ClosureQ(DBM_Q))
  (Octagonal_rewriting.RewriterQF)

module DBM_F = Dbm.Make(Bound_float)
module OctagonF = Make
  (Bound_float)
  (FloatIntervalDBM)
  (Closure.ClosureF(DBM_F))
  (Octagonal_rewriting.RewriterQF)