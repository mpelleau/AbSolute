(* This file implements the abstract domain of Octagon.
   It relies on the technique described in the dissertation of Marie Pelleau, Chapter 5 (2012).
   In particular, it is based on the observation that a 2D octagon can be represented by the intersection of two boxes, where one box is turned at 45° with respect to the other.
   It generalizes to dimension N with N^2 boxes (every box on (i,j)-plan is turned).
   This is useful because usual interval constraint propagators can be used.
   In addition, we have additional pruning with the Floyd Warshall algorithm filtering the octagonal constraints.
*)

open Adcp_sig
open Csp
open Abstract_box

(* Documentation taken from the APRON project.

   We consider matrices of 2n*2n upper bounds.
   Let us denote by (i,j) the matrix element at line i, column j; the matrix
   induces the following constraints:
     Vj-Vi <= (2i,2j)
     Vj+Vi <= (2i+1,2j)
    -Vj-Vi <= (2i,2j+1)
    -Vj+Vi <= (2i+1,2j+1)

   Actually, this representation is redudant, and so, we manipulate
   2x2 block lower-triangular matrices.
   Only elements (i,j) such that j/2 <= i/2 are represented:

       j ->  0 1 2 3 4 5
            ___
        0  |_|_|
        1  |_|_|___
  i ->  2  |_|_|_|_|
        3  |_|_|_|_|___
        4  |_|_|_|_|_|_|
        5  |_|_|_|_|_|_|


                 j
             0     -2x0
            2x0      0
       i
           x0-x1  -x0-x1      0   -2x1
           x0+x1  -x0+x1     2x1    0

   Elements such that j/2 > i/2 are retreived by coherence: (i,j) = (j^1,i^1)
*)

module BoxedOctagon = struct
  module F = Bound_float
  type bound = F.t
  type m = bound array
  module I = Trigo.Make(Itv.ItvF)
  module B = Box(I)

  module Env = Tools.VarMap
  (* maps each variable to its index `i` in the matrix such that dbm[i,i] is the constraint vi - vi <= c *)
  type e = int Env.t
  type t = {
    env : e;
    (* Number of variables *)
    dim: int;
    (* difference bound matrix *)
    dbm : bound array;
    (* Interval representation of the octagon (just the base variables). *)
    box : B.t
  }

  let matsize : int -> int
    = fun dim -> 2 * dim * (dim + 1)

  (* position of (i,j) element, assuming j/2 <= i/2 *)
  let matpos : int -> int -> int
    = fun i j -> j + ((i+1)*(i+1))/2

  (* position of (i,j) element, no assumption *)
  let matpos2 : int -> int -> int = fun i j ->
    if j > i then matpos (j lxor 1) (i lxor 1)
    else matpos i j

  let ub_idx : int -> int = fun dim -> matpos2 (dim*2+1) (dim*2)
  let lb_idx : int -> int = fun dim -> matpos2 (dim*2) (dim*2+1)

  let ub : t -> int -> bound = fun o dim ->
    F.div_up o.dbm.(ub_idx dim) 2.

  let lb : t -> int -> bound = fun o dim ->
    F.div_down o.dbm.(lb_idx dim) (-2.)

  let set_ub : t -> int -> bound -> unit = fun o dim v ->
    o.dbm.(ub_idx dim) <- F.mul_up v 2.

  let set_lb : t -> int -> bound -> unit = fun o dim v ->
    o.dbm.(ub_idx dim) <- F.mul_down v (-2.)

  (* returns an empty element *)
  let empty : t = { env=Env.empty; dim=0; dbm=[||]; box=B.empty }

  let copy : t -> t = fun o ->
    { env = o.env; dim=o.dim; dbm=Array.copy o.dbm; box=o.box }

  (* returns the variables *)
  let vars : t -> (Csp.annot * Csp.var) list = fun o ->
    Env.fold (fun k _ acc -> (Real, k)::acc) o.env []

  let check_type : Csp.annot -> unit = fun typ ->
    match typ with
    | Int -> Pervasives.failwith "BoxedOctagon: only support real variables."
    | Real -> ()

  (* adds an unconstrained variable to the octagon *)
  let add_var : t -> Csp.annot * Csp.var -> t = fun o (typ,var) ->
    let o = copy o in
    (* increase the dimension. *)
    check_type typ;
    let dim' = o.dim + 1 in
    (* map the dimension to the variable name in the environment. *)
    let env' = Env.add var dim' o.env in
    (* allocate in the matrix two rows of size 2*dim' with a top bound. *)
    let top = F.inf in
    let row = Array.make (dim'*2*2) top in
    let dbm' = Array.append o.dbm row in
    let box' = B.add_var o.box (typ, var) in
    { env=env'; dim=dim'; dbm=dbm'; box=box' }

  let var_bounds_from_dim : t -> int -> (Mpqf.t * Mpqf.t) = fun o dim ->
    (F.to_rat (lb o dim), F.to_rat (ub o dim))

  (* returns the bounds of a variable *)
  let var_bounds : t -> Csp.var -> (Mpqf.t * Mpqf.t) = fun o var ->
    let dim = Env.find var o.env in
    var_bounds_from_dim o dim

  (* returns the bound variables *)
  let bound_vars : t -> Csp.csts
    = fun o -> []

  (* removes an unconstrained variable from the environnement *)
  let rem_var : t -> Csp.var -> t
    = fun o _ -> Pervasives.failwith "BoxedOctagon: function `rem_var` unimplemented."

  (*** PREDICATES ***)

  let volume : t -> float
    = fun o -> 0.

  (* tests if an abstract element is too small to be cut *)
  let is_small : t -> bool
    = fun o -> volume o <= !Constant.precision

  let is_failed : t -> bool
    = fun o -> Array.exists (fun x -> x < F.zero) o.dbm

  let is_empty : t -> bool
    = fun o -> Env.is_empty o.env || is_failed o

  let check_same_env : t -> t -> unit = fun large small ->
    let checker = fun k v ->
      if Env.find k large.env <> v then
        raise Not_found
    in
    try
      Env.iter checker small.env
    with Not_found ->
      Pervasives.failwith "check_same_env: try to join two octagons s.t.\
                           their environments is not included in the other."

  (*** OPERATIONS ***)
  let join : t -> t -> t = fun o o' ->
    let (res, rest) =
      if Array.length o.dbm >= Array.length o'.dbm then
        (o, o')
      else
        (o', o)
    in
    let res = copy res in
    let res = { res with box=(B.join res.box rest.box) } in
    check_same_env res rest;
    let merge = fun idx x -> res.dbm.(idx) <- (F.max x res.dbm.(idx)) in
    Array.iteri merge rest.dbm;
    res

  let meet_dbm_into_box : t -> t = fun o ->
    let filter_box = fun k dim box ->
      let (l,u) = var_bounds_from_dim o dim in
      let box = B.filter box (Var k, GEQ, Cst (l, Real)) in
      B.filter box (Var k, LEQ, Cst (u, Real)) in
    { o with box=Env.fold filter_box o.env o.box }

  let meet_box_into_dbm : t -> t = fun o ->
    let update_cell = fun k dim ->
      let (l, u) = B.float_bounds o.box k in
      set_lb o dim l;
      set_ub o dim u;
    in
    Env.iter update_cell o.env;
    o

  let bound_add : bound -> bound -> bound = fun x y ->
    if x = F.inf || y = F.inf then
      F.inf
    else
      F.add_up x y

  (* Floyd-Warshall algorithm: it propagates the octogonal constraints *)
  let floyd_warshall_for_octagon : t -> t = fun o ->
    let o = copy o in
    let m = fun i j -> o.dbm.(matpos2 (i-1) (j-1)) in
    let n = o.dim in
    for k = 1 to n do
      for i = 1 to (2*n) do
        for j = 1 to (2*n) do
          let v = List.fold_left F.min F.inf [
            bound_add (m i (2*k)) (m (2*k) j) ;
            bound_add (m i (2*k-1)) (m (2*k-1) j) ;
            bound_add (bound_add (m i (2*k-1)) (m (2*k -1) (2*k))) (m (2*k) j) ;
            bound_add (bound_add (m i (2*k)) (m (2*k) (2*k-1))) (m (2*k-1) j) ] in
          if v < m i j then
            o.dbm.(matpos2 (i-1) (j-1)) <- v
        done
      done;
      for i = 1 to (2*n) do
        for j = 1 to (2*n) do
          let i' = if i mod 2 = 0 then i - 1 else i + 1 in
          let j' = if j mod 2 = 0 then j - 1 else j + 1 in
          if (bound_add (m i i')  (m j' j)) < (m i j) then
            o.dbm.(matpos (i-1) (j-1)) <- (bound_add (m i i') (m j' j))
        done
      done
    done;
    for i = 1 to (2*n) do
      if m i i < 0. then
        raise Bot.Bot_found
      else
        o.dbm.(matpos2 (i-1) (i-1)) <- 0.
    done;
    meet_dbm_into_box o

  (* pruning *)
  let prune : t -> t -> t list * t
    = fun o o' -> Pervasives.failwith "BoxedOctagon: function `prune` unimplemented."

  (* splits an abstract element *)
  let split : t -> t list
    = fun o -> []

  (* Throw Bot.Bot_found if an inconsistent abstract element is reached. *)
  (* This filter procedure is currently very inefficient since we perform the closure (with Floyd-Warshall) every time we call a filtering on a constraint.
     However, performing the better algorithm presented in (Pelleau, Chapter 5, 2012) requires we have all the constraints to filter at once. *)
  let filter : t -> (Csp.expr * Csp.cmpop * Csp.expr) -> t = fun o cons ->
    let box' = B.filter o.box cons in
    let o' = { o with box=box' } in
    let o' = meet_box_into_dbm o' in
    let o' = floyd_warshall_for_octagon o' in
    meet_dbm_into_box o'

  let forward_eval : t -> Csp.expr -> (Mpqf.t * Mpqf.t)
    = fun o e -> Pervasives.failwith "BoxedOctagon: function `forward_eval` unimplemented."

  (* transforms an abstract element in constraints *)
  let to_bexpr : t -> (Csp.expr * Csp.cmpop * Csp.expr) list
    = fun o -> Pervasives.failwith "BoxedOctagon: function `to_bexpr` unimplemented."

  (* check if a constraint is suited for this abstract domain *)
  let is_representable : Csp.bexpr -> answer =
    fun _ -> Adcp_sig.Yes

  let print : Format.formatter -> t -> unit
    = fun fmt o -> Pervasives.failwith "BoxedOctagon: function `print` unimplemented."

  (* concretization function. we call it a spawner.
     useful to do tests, and to reuse the results.
     values are generated randomly *)
  let spawn : t -> Csp.instance
    = fun o -> Pervasives.failwith "BoxedOctagon: function `spawn` unimplemented."

  (* check if an abstract element is an abstractin of an instance *)
  let is_abstraction : t -> Csp.instance -> bool
    = fun o vars -> Pervasives.failwith "BoxedOctagon: function `is_abstraction` unimplemented."

end
