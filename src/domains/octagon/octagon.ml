open Csp
open Interval_view_dbm
open Octagonal_rewriting
open Abstract_domain

module type Octagon_sig =
sig
  type t
  type bound
  val init: Csp.var list -> Csp.bconstraint list -> ((bool * Csp.bconstraint) list * t)
  val empty: t
  val copy: t -> t
  val extend_one: t -> Csp.var -> t
  val update: t -> Octagonal_rewriting.octagonal_constraint -> unit
  val meet_constraint: t -> Csp.bconstraint -> bool
  val fold_vars: (var -> dbm_key -> 'a -> 'a) -> 'a -> t -> 'a
  val iter_vars: (var -> dbm_key -> unit) -> t -> unit
  val set_lb: t -> dbm_key -> bound -> unit
  val set_ub: t -> dbm_key -> bound -> unit
  val lb: t -> dbm_key -> bound
  val ub: t -> dbm_key -> bound
  val closure: t -> unit
  val dbm_as_list: t -> bound list
  val entailment: t -> octagonal_constraint -> kleene
end

module Make
  (B: Bound_sig.BOUND)
  (IntervalView: IntervalViewDBM with type bound=B.t)
  (Closure: Closure.Closure_sig with module DBM = Dbm.Make(B))
  (Rewriter: Octagonal_rewriting.Rewriter_sig) =
struct
  module DBM = Closure.DBM

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

  let index_of octagon (sign, v) =
    let (d,_) = Env.find v octagon.env in
    match sign with
    | Positive -> d*2+1
    | Negative -> d*2

  let update octagon oc =
    let index_of = index_of octagon in
    DBM.set octagon.dbm (index_of oc.x, index_of oc.y) (B.of_rat_up oc.c)

  let meet_constraint octagon c =
    let iter_oct = List.iter (update octagon) in
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

  (** Reexported functions from the parametrized modules. *)
  let closure octagon = Closure.closure octagon.dbm
  let is_consistent octagon = Closure.is_consistent octagon.dbm
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