(* This file implements the abstract domain of Octagon.
   It relies on the technique described in the dissertation of Marie Pelleau, Chapter 5 (2012).
   In particular, it is based on the observation that a 2D octagon can be represented by the intersection of two boxes, where one box is turned at 45° with respect to the other.
   It generalizes to dimension N with N^2 boxes (every box on (i,j)-plan is turned).
   This is useful because usual interval constraint propagators can be used in these rotated box.
   In addition, we have additional pruning with the Floyd Warshall algorithm filtering the octagonal constraints.
*)

(* open Adcp_sig *)
open Csp
open Abstract_box
open Bound_sig

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

type octagonalisation_kind =
    ConstraintBased
  | Random
  | StrongestLink
  | Promising

type var_order =
    LargestFirst
  | LargestCanFirst
  | LargestOctFirst
  | OctSplit

let boct_split : var_order ref = ref LargestFirst
let octagonalisation : octagonalisation_kind ref = ref Random

let set_split
    = function
    | "lf" -> boct_split := LargestFirst
    | "lcf" -> boct_split := LargestCanFirst
    | "lof" -> boct_split := LargestOctFirst
    | "os" -> boct_split := OctSplit
    | s -> "Splitting strategy " ^ s ^ " is undefined. Should be among: lf, lcf, lof, os" |> invalid_arg

let set_octagonalisation =
    function
    | "cb" -> octagonalisation := ConstraintBased
    | "random" -> octagonalisation := Random
    | "sl" -> octagonalisation := StrongestLink
    | "promising" -> octagonalisation := Promising
    | s -> "Octagonalisation strategy " ^ s ^ " is undefined. Should be among: cb, random, sl, promising" |> invalid_arg

(* binary constraint *)
type bconstraint = (expr * cmpop * expr)

type plane = int * int
(* An element in the DBM is uniquely identified by its plane and dimension. *)
type key = int * plane

(* If the variable `v` is rotated in the plane `(d1,d2)` then returns `then_b`, otherwise `else_b`. *)
let if_rotated_else : key -> 'a -> 'a -> 'a = fun (v, (d1,d2)) then_b else_b ->
  if d1 <> d2 && (d1 = v || d2 = v) then then_b else else_b

(* This module signature provides function to view DBM's values as interval bounds.
   Rational: We must extract this code from `BoxedOctagon` because it depends on the bound type. *)
module type IntervalViewDBM = sig
  type bound
  type itv

  val dbm_to_lb : key -> bound -> bound
  val dbm_to_ub : key -> bound -> bound
  val lb_to_dbm : key -> bound -> bound
  val ub_to_dbm : key -> bound -> bound

  val itv_to_range : itv -> (bound * bound)
  val range_to_itv : bound -> bound -> itv
end

module FloatIntervalDBM = struct
  module B = Bound_float
  module I = Trigo.Make(Itv.Itv(B))
  type bound = B.t
  type itv = I.t

  (* Rules for coping with rounding when transferring from DBM to BOX:
    * From BOX to DBM: every number is rounded UP because these numbers only decrease during the Floyd Warshall algorithm.
    * From DBM to BOX: the number is rounded DOWN for lower bound and UP for upper bound.

   To simplify the treatment (and improve soundness), we use interval arithmetic: (sqrt 2) is interpreted as the interval [sqrt_down 2, sqrt_up 2].
   Further operations are performed on this interval, and we chose the lower or upper bound at the end depending on what we need.
  *)
  let two_it = I.of_float (B.two)
  let minus_two_it = I.neg two_it
  let sqrt2_it = I.of_floats (B.sqrt_down B.two) (B.sqrt_up B.two)
  let minus_sqrt2_it = I.neg sqrt2_it
  let lb_it i = let (l,_) = I.to_float_range i in l
  let ub_it i = let (_,u) = I.to_float_range i in u

  let dbm_to_lb : key -> bound -> bound = fun k v ->
    let vi = I.of_float v in
    let divider = if_rotated_else k minus_sqrt2_it minus_two_it in
    lb_it (Bot.nobot (I.div vi divider))

  let dbm_to_ub : key -> bound -> bound = fun k v ->
    let vi = I.of_float v in
    let divider = if_rotated_else k sqrt2_it two_it in
    ub_it (Bot.nobot (I.div vi divider))

  let lb_to_dbm : key -> bound -> bound = fun k v ->
    let multiplier = if_rotated_else k minus_sqrt2_it minus_two_it in
    lb_it (I.mul (I.of_float v) multiplier)

  let ub_to_dbm : key -> bound -> bound = fun k v ->
    let multiplier = if_rotated_else k sqrt2_it two_it in
    ub_it (I.mul (I.of_float v) multiplier)

  let itv_to_range : itv -> (bound * bound) = I.to_float_range
  let range_to_itv : bound -> bound -> itv = I.of_floats
end

module RationalIntervalDBM = struct
  module B = Bound_mpqf
  module I = Trigo.Make(Itv.Itv(B))
  type bound = B.t
  type itv = I.t

  let of_int : int -> bound = Bound_mpqf.of_int_up

  let sqrt2_it = I.of_rats (B.sqrt_down B.two) (B.sqrt_up B.two)
  let minus_sqrt2_it = I.neg sqrt2_it
  let lb_it i = let (l,_) = I.to_rational_range i in l
  let ub_it i = let (_,u) = I.to_rational_range i in u

  let dbm_to_lb : key -> bound -> bound = fun k v ->
    let vi = I.of_rat v in
    let in_plane = lb_it (Bot.nobot (I.div vi minus_sqrt2_it)) in
    if_rotated_else k in_plane (B.div_down v B.minus_two)

  let dbm_to_ub : key -> bound -> bound = fun k v ->
    let vi = I.of_rat v in
    let in_plane = ub_it (Bot.nobot (I.div vi sqrt2_it)) in
    if_rotated_else k in_plane (B.div_up v B.two)

  let lb_to_dbm : key -> bound -> bound = fun k v ->
    let in_plane = lb_it (I.mul (I.of_rat v) minus_sqrt2_it) in
    if_rotated_else k in_plane (B.mul_down v B.minus_two)

  let ub_to_dbm : key -> bound -> bound = fun k v ->
    let in_plane = ub_it (I.mul (I.of_rat v) sqrt2_it) in
    if_rotated_else k in_plane (B.mul_up v B.two)

  let itv_to_range : itv -> (bound * bound) = I.to_rational_range
  let range_to_itv : bound -> bound -> itv = I.of_rats
end

module IntegerIntervalDBM = struct
  module B = Bound_int
  module I = Trigo.Make(Itv.Itv(B))
  type bound = B.t
  type itv = I.t

  let dbm_to_lb : key -> bound -> bound = fun k v ->
    B.of_rat_down (RationalIntervalDBM.dbm_to_lb k (RationalIntervalDBM.of_int v))
  let dbm_to_ub : key -> bound -> bound = fun k v ->
    B.of_rat_up (RationalIntervalDBM.dbm_to_lb k (RationalIntervalDBM.of_int v))
  let lb_to_dbm : key -> bound -> bound = fun k v ->
    B.of_rat_up (RationalIntervalDBM.lb_to_dbm k (RationalIntervalDBM.of_int v))
  let ub_to_dbm : key -> bound -> bound = fun k v ->
    B.of_rat_up (RationalIntervalDBM.ub_to_dbm k (RationalIntervalDBM.of_int v))

  let itv_to_range : itv -> (bound * bound) = fun (l,u) ->
    let (l,u) = I.to_float_range (l,u) in
    (int_of_float l, int_of_float (ceil u))

  let range_to_itv : bound -> bound -> itv = I.of_ints
end

module BoxedOctagon
  (B:BOUND)
  (ItvDBM:IntervalViewDBM with
    type bound = B.t and
    type itv = Trigo.Make(Itv.Itv(B)).t)
= struct
  include ItvDBM

  module B = B
  type m = bound array
  module I = Trigo.Make(Itv.Itv(B))
  module Box = Box(I)

  module Env = Tools.VarMap
  module REnv = Mapext.Make(struct
    type t=key
    let compare = compare end)
  type e = key Env.t
  type re = string REnv.t
  (* Invariants:
      * For all key, we have: `key = REnv.find (Env.find env key) renv`
      * `Box.length box = (dim + 2 * (List.length planes))`
  *)
  type t = {
    (* maps each variable name to its `key` in the dbm. *)
    env : e;
    (* reversed mapping of `env`. *)
    renv : re;
    (* Number of variables in the canonical box. *)
    dim: int;
    (* difference bound matrix. *)
    dbm : bound array;
    (* Interval representation of the octagon. *)
    box : Box.t;
    (* List of the planes registered in `box` (it depends on the octagonalisation strategy).
       Note: the canonical plane is not registered in `planes`. *)
    planes: plane list
  }

  (* Canonical plane *)
  let cplane = (0,0)

  (* position of (i,j) element, assuming j/2 <= i/2 *)
  let matpos : int -> int -> int
    = fun i j -> j + ((i+1)*(i+1))/2

  (* position of (i,j) element, no assumption *)
  let matpos2 : int -> int -> int = fun i j ->
    (* Coherence *)
    if j > i then matpos (j lxor 1) (i lxor 1)
    else matpos i j

  let well_formed_plane : plane -> bool = fun (d1,d2) ->
    ((d1 = 0 || d1 <> d2) && d1 <= d2)

  (* We represent the canonical plane (where d1=d2) only with (0,0). *)
  let check_well_formed_plane : plane -> unit = fun plane ->
    assert (well_formed_plane plane)

  (* position of the lower bound element of the variable `v` in the canonical or rotated `plane`. *)
  let lb_pos : key -> int = fun (v, (d1, d2)) ->
    check_well_formed_plane (d1,d2);
    if v = d1 && d1 <> d2 then matpos2 (2*d2) (2*d1+1)
    else if v = d2 && d1 <> d2 then matpos2 (2*d2) (2*d1)
    (* Non-rotated dimension, or canonical plane. *)
    else matpos2 (v*2) (v*2+1)

  (* Same as `lb_pos` but for the upper bound. *)
  let ub_pos : key -> int = fun (v, (d1,d2)) ->
    check_well_formed_plane (d1,d2);
    if v = d1 && d1 <> d2 then matpos2 (2*d2+1) (2*d1)
    else if v = d2 && d1 <> d2 then matpos2 (2*d2+1) (2*d1+1)
    else matpos2 (v*2+1) (v*2)

  let print : Format.formatter -> t -> unit = fun fmt o ->
    Format.fprintf fmt "Box representation: ";
    Box.print fmt o.box;
    Format.fprintf fmt "\nMatrix representation:\n";
    let n = o.dim in
    for i = 0 to (2*n-1) do
      for j = 0 to (2*n-1) do
        Format.fprintf fmt "%a " Format.pp_print_float (B.to_float_up (o.dbm.(matpos2 i j)))
      done;
      Format.fprintf fmt "\n"
    done

  let print_out : t -> unit = fun o -> Format.printf "%a" print o

  (* returns an empty element *)
  let empty : t = { env=Env.empty; renv=REnv.empty; dim=0; dbm=[||]; box=Box.empty; planes=[] }

  (* This function copy the imperative data structure of the octagon to create a fresh one.
     It is necessary for the search algorithm which backtracks since `dbm` is mutable (and thus not automatically copied). *)
  let copy : t -> t = fun o ->
    { env = o.env; renv=o.renv; dim=o.dim; dbm=Array.copy o.dbm; box=o.box; planes=o.planes }

  let length : t -> int = fun o -> o.dim
  let dbm_length : t -> int = fun o -> Array.length o.dbm
  let planes_length : t -> int = fun o -> List.length o.planes

  let is_empty : t -> bool = fun o -> (length o) = 0

  (* Allocate in the matrix two rows of size 2*dim' with an infinite bound. *)
  let add_var_in_dbm : t -> t = fun o ->
    let o = copy o in
    (* increase the dimension. *)
    let dim' = o.dim + 1 in
    let top = B.inf in
    let row = Array.make (dim'*2*2) top in
    let dbm' = Array.append o.dbm row in
    { o with dim=dim'; dbm=dbm' }

  (* Register the `key` of the variable `var` in the environment. *)
  let add_var_in_box : t -> Csp.annot -> Csp.var -> key -> t = fun o typ var key ->
    let env' = Env.add var key o.env in
    let renv' = REnv.add key var o.renv in
    let box' = Box.add_var o.box (typ, var) in
    { o with env=env'; renv=renv'; box=box' }

  let add_var : t -> Csp.annot -> Csp.var -> t = fun o typ var ->
    let o = add_var_in_dbm o in
    let key = (o.dim-1, cplane) in
    add_var_in_box o typ var key

  let key_of : t -> var -> key = fun o name -> Env.find name o.env

  (* Monotonic write: We set a value in the DBM only if the current one is larger. *)
  let set : t -> int -> bound -> unit = fun o pos v ->
    if B.gt o.dbm.(pos) v then
      o.dbm.(pos) <- v

  (* Set the lower bound of the variable `k` in the DBM.
     The value `v` is the lower bound of the value at key `k`, and is processed to fit in the DBM. *)
  let set_lb : t -> key -> bound -> unit = fun o k v ->
    set o (lb_pos k) (lb_to_dbm k v)

  (* Same as `set_lb` but for the upper bound. *)
  let set_ub : t -> key -> bound -> unit = fun o k v ->
    set o (ub_pos k) (ub_to_dbm k v)

  (* Lower bound of the variable at key `k`.
     The value is computed directly from the DBM with care on rounding. *)
  let lb : t -> key -> bound = fun o k -> dbm_to_lb k o.dbm.(lb_pos k)

  (* Same as `lb` but for the upper bound. *)
  let ub : t -> key -> bound = fun o k -> dbm_to_ub k o.dbm.(ub_pos k)

  let lb' : t -> var -> bound = fun o name -> lb o (key_of o name)
  let ub' : t -> var -> bound = fun o name -> ub o (key_of o name)

  (* Returns the bounds of the variable `k` in `plane` (as currently set in the DBM). *)
  let bounds_of_var : t -> key -> (Mpqf.t * Mpqf.t) = fun o key ->
    (B.to_rat (lb o key), B.to_rat (ub o key))

  (* Returns the bounds of the variable `var`.
     See also `bounds_of_var`. *)
  let var_bounds : t -> Csp.var -> (Mpqf.t * Mpqf.t) = fun o var ->
    let key = Env.find var o.env in
    bounds_of_var o key

  let internal_suffix = "_BoxedOctagon_internal"
  let internal_name canonical_name d1 d2 =
    canonical_name ^ "_in_" ^ (string_of_int d1) ^ "_" ^ (string_of_int d2) ^ internal_suffix

  let var_info : t -> key -> (Csp.annot * Csp.var) = fun o (v, (d1,d2)) ->
    try
    let canonical_name = REnv.find (v, cplane) o.renv in
    let typ = if Box.is_integer o.box canonical_name then Int else Real in
    if (d1,d2) = cplane then
      (typ, canonical_name)
    else
      (typ, internal_name canonical_name d1 d2)
    with Not_found -> failwith ("var_info: cannot find variable " ^ (string_of_int v))

  let check_add_plane : t -> plane -> unit = fun o (d1,d2) ->
    check_well_formed_plane (d1,d2);
    assert (o.dim > d1 && o.dim > d2 && d1 <> d2);
    assert (not (List.mem (d1,d2) o.planes))

  let add_plane : t -> plane -> t = fun o (d1,d2) ->
    let plane = (d1,d2) in
    check_add_plane o plane;
    let add o key =
      let (typ, name) = var_info o key in
      add_var_in_box o typ name key in
    let o = add o (d1, plane) in
    let o = add o (d2, plane) in
    { o with planes=plane::o.planes }

  (* Add a box rotated in a random plane (d1, d2) such that 0 <= d1 < d2 < length o.
     Precondition: `o.dim > 1`
  *)
  let random_octagonalisation : t -> t = fun o ->
    begin
      Random.init 0;
      let d1 = (Random.int (o.dim - 1)) in
      let d2 = (Random.int (o.dim - d1 - 1)) + d1 + 1 in
      add_plane o (d1,d2)
    end

  (* Precondition: This function should be called after all the unconstrained variables have been added, and ideally (but not necessarily) before any constraint is filtered. *)
  let init_octagonalise : t -> t = fun o ->
    if o.dim >= 2 && o.planes = [] then
      match !octagonalisation with
        Random -> random_octagonalisation o
      | _ -> Pervasives.failwith "BoxedOctagon: this split is not implemented; only LargestFirst (lf) is currently implemented."
    else o

  (* It returns all variables including those created for the planes. *)
  let all_vars : t -> (Csp.annot * Csp.var) list = fun o -> Box.vars o.box

  let is_canonical : t -> var -> bool = fun o v ->
    let (_,plane) = (Env.find v o.env) in
    plane = cplane

  (* Returns the variables registered in the octagon `o`.
     It does not return the variables created by the octagon (those on planes).
  *)
  let vars : t -> (Csp.annot * Csp.var) list = fun o ->
    List.filter
      (fun (_, v) -> is_canonical o v)
      (all_vars o)

  (* Returns the variables instantiated (the lower bound is exactly equal to the upper bound).
     It does not return the variables instantiated in a plane.
  *)
  let bound_vars : t -> Csp.csts = fun o ->
    List.filter
      (fun (v, _) -> is_canonical o v)
      (Box.bound_vars o.box)

  (* Removes an unconstrained variable from the environment. *)
  let rem_var : t -> Csp.var -> t = fun o _ -> o

  (* Symbolic representation of the square root of 2.
     If it is computed right away, we do not know in what direction we should round the square root.
   *)
  let sym_sqrt2 = Funcall ("sqrt", [(Cst (Mpqf.of_int 2, Real))])

  (* To rotate the variables `(v1,v2)` in the plane they describe, we perform:
       v1 -> cos(45°)*v1 - sin(45°)*v2
       v2 -> sin(45°)*v1 + cos(45°)*v2
      Note that we have cos(45°) = sin(45°) = 1/(sqrt 2).
  *)
  let symbolic_var_rotation : (var * var) -> (expr * expr) = fun (v1,v2) ->
    let left = Binary (DIV, Var v1, sym_sqrt2) in
    let right = Binary (DIV, Var v2, sym_sqrt2) in
    let rv1 = Binary (SUB, left, right) in
    let rv2 = Binary (ADD, left, right) in
    (rv1, rv2)

  (* To rotate a constraint in the plane `(d1,d2)`:
      1. We rotate the variables at dimensions `d1` and `d2` (see `symbolic_var_rotation`).
      2. We replace every occurrence of these variables in the constraint.
      3. If they do not occur we return None, otherwise the rotated constraint.
  *)
  let rotate_constraint : t -> bconstraint -> plane -> bconstraint option = fun o (e1,op,e2) (d1,d2) ->
    let get_names di =
      let cvi = REnv.find (di, cplane) o.renv in
      let vi = REnv.find (di, (d1,d2)) o.renv in
      (cvi, vi) in
    let (cv1, v1) = get_names d1 in
    let (cv2, v2) = get_names d2 in
    let (rv1, rv2) = symbolic_var_rotation (v1, v2) in
    let found_var = ref false in
    let replace = fun v ->
      if v = cv1 || v = cv2 then begin
        found_var := true;
        if v = cv1 then rv1 else rv2 end
      else Var v
    in
    let e1' = replace_var_in_expr replace e1 in
    let e2' = replace_var_in_expr replace e2 in
    if !found_var then Some (e1', op, e2') else None

  (* We update the DBM with the values contained in the box if they improve the current bound in the DBM. *)
  let meet_box_into_dbm : t -> t = fun o ->
    let update_cell_from_box = fun var_name k ->
      let it = Box.find var_name o.box in
      let (l, u) = itv_to_range it in
      set_lb o k l;
      set_ub o k u;
    in
    Env.iter update_cell_from_box o.env;
    o

  let meet_dbm_into_box : t -> t = fun o ->
    let filter_var = fun var_name key box ->
      let l = lb o key in
      let u = ub o key in
      if l > u then raise Bot.Bot_found;
      let i = range_to_itv l u in
      Box.meet_var box var_name i in
    { o with box=Env.fold filter_var o.env o.box }

  let print_cons name (e1, op, e2) =
    Format.printf "%s" name;
    Format.printf ": %a\n" Csp.print_bexpr (Csp.Cmp (op, e1, e2))

  let string_of_plane (x,y) = "(" ^ string_of_int x ^ "," ^ string_of_int y ^ ")"

  (* We filter the constraint `cons` in `o.box`.
     We do not merge the box into the DBM here.
  *)
  let filter_in_box : bconstraint -> t -> t = fun cons o ->
    let box' = Box.filter o.box cons in
    { o with box=box' }

  (* We filter the constraint in the rotated `plane`.
     The constraint is not filtered if none of its variables is rotated in `plane`.
     We do not merge the box into the DBM here. *)
  let filter_in_plane : bconstraint -> t -> plane -> t = fun cons o plane ->
    match rotate_constraint o cons plane with
    | None -> o
    | Some rcons -> filter_in_box rcons o

  let filter_box : t -> bconstraint -> t = fun o cons ->
    let filter_rotated o = List.fold_left (filter_in_plane cons) o o.planes in
    (* filter in the canonical plane. *)
    o |>
    filter_in_box cons |>
    filter_rotated |>
    meet_box_into_dbm

  (* We raise `Bot_found` if there is a negative cycle (v < 0).
     Otherwise we set the value `v` in the DBM[i,j] if it improves the current value. *)
  let update_cell : t -> int -> int -> bound -> unit = fun o i j v ->
    let idx = matpos2 i j in
    set o idx v
    (* Possible improvement: reschedule the corresponding propagators. *)

  (* The part of the modified Floyd-Warshall algorithm for a given 'k'. *)
  let strong_closure_k : t -> int -> unit = fun o k ->
    let m = fun i j -> o.dbm.(matpos2 i j) in
    let n = o.dim in
    for i = 0 to (2*n-1) do
      for j = 0 to (2*n-1) do
        let v = List.fold_left B.min (m i j) [
          B.add_up (m i (2*k+1)) (m (2*k+1) j) ;
          B.add_up (m i (2*k)) (m (2*k) j) ;
          B.add_up (B.add_up (m i (2*k)) (m (2*k) (2*k+1))) (m (2*k+1) j) ;
          B.add_up (B.add_up (m i (2*k+1)) (m (2*k+1) (2*k))) (m (2*k) j) ] in
        update_cell o i j v
      done
    done

  (* Strengthening propagates the octagonal constraints in the DBM. *)
  let strengthening : t -> unit = fun o ->
    let m = fun i j -> o.dbm.(matpos2 i j) in
    let n = o.dim in
    for i = 0 to (2*n-1) do
      for j = 0 to (2*n-1) do
        let i' = i lxor 1 in
        let j' = j lxor 1 in
        let v = (B.div_up (B.add_up (m i i') (m j' j)) B.two) in
        update_cell o i j v
      done
    done

  (* The Floyd-Warshall algorithm. *)
  let floyd_warshall : t -> unit = fun o ->
    let m = fun i j -> o.dbm.(matpos2 i j) in
    let n = o.dim in
    for k = 0 to (2*n-1) do
      for i = 0 to (2*n-1) do
        for j = 0 to (2*n-1) do
          let v = B.min (m i j) (B.add_up (m i k) (m k j)) in
          update_cell o i j v
        done
      done
    done

  let rec propagate_neg : expr -> expr = fun e ->
    let neg e = Unary (NEG, e) in
    match e with
    | Unary (NEG, Cst(c,a)) -> Cst (Mpqf.neg c,a)
    | Unary (NEG, Unary (NEG, e)) -> propagate_neg e
    | Unary (NEG, Binary (SUB, x, y)) -> propagate_neg (Binary (ADD, neg x, y))
    | Unary (NEG, Binary (ADD, x, y)) -> propagate_neg (Binary (SUB, neg x, y))
    | Unary (op, x) -> Unary(op, propagate_neg x)
    | Binary (SUB, x, Unary (NEG, y)) -> propagate_neg (Binary (ADD, x, y))
    | Binary (ADD, x, Unary (NEG, y)) -> propagate_neg (Binary (SUB, x, y))
    | Binary (op, x, y) -> Binary (op, propagate_neg x, propagate_neg y)
    | e -> e

  let rec normalize : bconstraint -> bconstraint = fun (e1, op, e2) ->
    let prop_neg e1 op e2 = ((propagate_neg e1), op, (propagate_neg e2)) in
    let (e1, op, e2) = prop_neg e1 op e2 in
    match (e1, op, e2) with
    | e1, GEQ, Cst (c,a) -> normalize (Unary (NEG, e1), LEQ, Unary (NEG, Cst(c,a)))
    (* Relaxing e1 < e2 and e1 > e2 to e1 <= e2 and e1 >= e2 *)
    | e1, LT, e2 -> normalize (e1, LEQ, e2)
    | e1, GT, e2 -> normalize (e1, GEQ, e2)
    | _ -> prop_neg e1 op e2

  (* Returns `Some (x,y,c)` where the value `c` in the DBM at position of `x - y <= c` if `(e1,op,e2)` is an octagonal constraint, otherwise `None`.
     This function performs a simple pattern matching on the shape of the constraint: we do not attempt to symbolically rewrite the constraint.
  *)
  let extract_octagonal_cons : (var -> 'a) -> (var -> 'a) -> bconstraint -> ('a * 'a * bound) option = fun posi nega (e1,op,e2) ->
    let bound_of c = B.of_rat_up c in
    let open Csp in
    let (e1, op, e2) = normalize (e1, op, e2) in
    match e1, op, e2 with
    | Binary (op2, x, y), LEQ, Cst (c, _) ->
        let c = bound_of c in
        (match op2, x, y with
        (* x - y <= c *)
        | SUB, Var x, Var y
        | ADD, Var x, Unary (NEG, Var y) -> Some (posi x, posi y, c)
        (* -x - y <= c *)
        | SUB, Unary (NEG, Var x), Var y
        | ADD, Unary (NEG, Var x), Unary (NEG, Var y) -> Some (nega x, posi y, c)
        (* x - -y <= c *)
        | SUB, Var x, Unary (NEG, Var y)
        | ADD, Var x, Var y -> Some (posi x, nega y, c)
        (* -x - -y <= c *)
        | SUB, Unary (NEG, Var x), Unary (NEG, Var y)
        | ADD, Unary (NEG, Var x), Var y -> Some (nega x, nega y, c)
        | _ -> None
        )
    (* x <= c *)
    | Var x, LEQ, Cst (c, _) -> Some (posi x, nega x, (B.mul_up (bound_of c) B.two))
    (* -x <= c *)
    | Unary (NEG, Var x), LEQ, Cst (c, _) -> Some (nega x, posi x, (B.mul_up (bound_of c) B.two))
    | _ -> None

  let filter_octagonal : t -> bconstraint -> (t * bool) = fun o cons ->
    let dim_of x = let (dim, _) = Env.find x o.env in dim in
    let posi x = (dim_of x) * 2 in
    let nega x = (dim_of x) * 2 + 1 in
    match extract_octagonal_cons posi nega cons with
    | Some (i,j,c) ->
        let o = copy o in
        set o (matpos2 j i) c;
        let o = meet_dbm_into_box o in
        (o, true)
    | None -> (o, false)

  let list_of_dbm : t -> bound list = fun o ->
    Array.to_list o.dbm

  (* True if the two representations (DBM and BOX) of `o` and `o'` are identical up to an epsilon.
     Precondition: o <= o' or o' <= o with `<=` the order on the lattice of octagons. *)
  let equal : t -> t -> bound -> bool = fun o o' epsilon ->
    let bound_equal x y =
      if B.neq x y then B.leq (B.abs (B.sub_up x y)) epsilon
      else true in
    let rec aux eq l1 l2 = match l1, l2 with
      | [], [] -> true
      | [], _ | _, [] -> false
      | x::xs, y::ys -> eq x y && aux eq xs ys in
    aux bound_equal (list_of_dbm o) (list_of_dbm o') &&
    bound_equal (B.of_float_up (Box.volume o.box)) (B.of_float_up (Box.volume o'.box))

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

  (* An octagons is a lattice, thus it has a join and meet operators.
     `join` performs the union of the two octagons `o` and `o'`, and `meet` their intersection.
     Precondition: `o` and `o'` are defined on the same set of variables (due to condition on `Abstract_box.join`).
     Consequently, they have the same octagonalisation strategy.
  *)
  let merge_with : t -> t -> (Box.t -> Box.t -> Box.t) -> (bound -> bound -> bound) -> t =
   fun o o' merge_box merge_bound ->
    if Array.length o.dbm <> Array.length o'.dbm then
      Pervasives.failwith "BoxedOctagon.join: two octagons not defined on the same set of variables."
    else ();
    check_same_env o o';
    let o = copy o in
    let merge = fun idx x -> o.dbm.(idx) <- (merge_bound x o.dbm.(idx)) in
    Array.iteri merge o'.dbm;
    { o with box=merge_box o.box o'.box }

  let join : t -> t -> t = fun o o' -> merge_with o o' Box.join B.max
  let meet : t -> t -> t = fun o o' -> merge_with o o' Box.meet B.min

  (* check if a constraint is suited for this abstract domain *)
  let rec is_representable : Csp.bexpr -> Adcp_sig.answer =
    let cons_is_representable cons =
      let id x = x in
      match extract_octagonal_cons id id cons with
      | Some (_) -> Adcp_sig.Yes
      | None -> Adcp_sig.Maybe in
    Adcp_sig.(function
    | Csp.Cmp (op, b1, b2) -> cons_is_representable (b1, op, b2)
    | Csp.And (e1, e2) -> and_ans (is_representable e1) (is_representable e2)
    | Csp.Or (e1, e2) -> and_ans (is_representable e1) (is_representable e2)
    | Csp.Not e -> is_representable e)

  (* We delegate this evaluation to the canonical box.
   * Possible improvement: obtain a better approximation by turning the expression and forward_eval it in different plane.
   *   We would then meet the results of all planes. *)
  let forward_eval : t -> Csp.expr -> (Mpqf.t * Mpqf.t) = fun o e ->
    Box.forward_eval o.box e

  let create_var_from_cell : t -> int -> expr = fun o i ->
    let x = REnv.find ((i/2), cplane) o.renv in
    if i mod 2 = 0 then
      Var x
    else
      Unary (NEG, Var x)

  let dbm_cell_to_bexpr : t -> int -> bconstraint list -> int -> bconstraint list = fun o i res j ->
    let open Csp in
    let idx = matpos i j in
    (* We do not generate the constraint where the coefficient is infinite. *)
    res@(
      if B.neq o.dbm.(idx) B.inf && i <> j then
        let x = create_var_from_cell o j in
        let y = create_var_from_cell o i in
        (* -x <= -c *)
        if i mod 2 = 0 && j mod 2 = 1 && i = j - 1 then
          let c = Cst ((Mpqf.div (B.to_rat o.dbm.(idx)) (Mpqf.of_int 2)), Real) in
          [(x,LEQ,c)]
        (* x <= c *)
        else if i mod 2 = 1 && j mod 2 = 0 && i = j + 1 then
          let c = Cst ((Mpqf.div (B.to_rat o.dbm.(idx)) (Mpqf.of_int 2)), Real) in
          [(x,LEQ,c)]
        (* x - y <= c *)
        else
          let c = Cst ((B.to_rat o.dbm.(idx)), Real) in
          let left = (Binary (SUB, x, y)) in (* NOTE: we could use `Csp.simplify_fp` but the problem is that the generated constraint does not match `is_representable` anymore. *)
          [(left,LEQ,c)]
      else
        [])

  (* Largest first split: select the biggest variable in all planes and split on its middle value.
   * We rely on the split of Box. *)
  let split_lf : t -> ctrs -> t list = fun o c ->
    let create_node box =
      let o' = copy o in
      let o' = { o' with box=box } in
      let o' = meet_box_into_dbm o' in
      o' in
    let boxes = Box.split o.box c in
    let splits = List.map create_node boxes in
    splits

  (* splits an abstract element *)
  let split : t -> ctrs -> t list = fun o c ->
    if !Constant.debug > 1 then Printf.printf "split";
    match !boct_split with
    | LargestFirst -> split_lf o c
    | _ -> Pervasives.failwith "BoxedOctagon: this split is not implemented; only LargestFirst (lf) is currently implemented."

  (* Extract the octagonal constraints from the DBM in `o`.
     We do not extract "useless" constraints such as `x - y <= inf`. *)
  let to_bexpr : t -> bconstraint list = fun o ->
    let aux res i =
      List.fold_left (dbm_cell_to_bexpr o i) res (Tools.range 0 (i lxor 1))
    in
    List.fold_left aux [] (Tools.range 0 ((2*o.dim)-1))

  let project_canonical_box : t -> Box.t = fun o ->
    Box.project (is_canonical o) o.box

  let volume : t -> float = fun o -> Box.volume (project_canonical_box o)

  (* tests if an abstract element is too small to be cut *)
  let is_small : t -> bool = fun o -> Box.is_small (project_canonical_box o)

  let prune : (t -> t -> t list) option = None

  let join_vars : t -> Csp.csts -> t = fun o vars ->
    let o = copy o in
    let set_bound (k, (l, u)) =
      let key = Env.find k o.env in
      set_lb o key (B.of_rat_down l);
      set_ub o key (B.of_rat_up u) in
    List.iter set_bound vars;
    o

  (* We check if `o` is an abstraction of `i` by adding `i` into the current octagon, and then check if it is consistent. *)
  let is_abstraction : t -> Csp.instance -> bool = fun o i ->
    if not (Box.is_abstraction (project_canonical_box o) i) then
      false
    else
      try
        let _ = join_vars o (List.map (fun (k,v) -> (k,(v,v))) (Tools.VarMap.bindings i)) in
        true
      with Bot.Bot_found -> false

  let spawn : t -> Csp.instance = fun o ->
    let rec try_spawn o d =
      if d >= (length o) then Pervasives.failwith "unreachable";
      let t = Box.spawn (project_canonical_box o) in
      if not (is_abstraction o t) then
        let key = (d, cplane) in
        let lb_v = lb o key in
        let ub_v = ub o key in
        let x = Mpqf.of_float (I.spawn (lb_v, ub_v)) in
        let (_,v) = var_info o key in
        let o = join_vars o [(v, (x,x))] in
        try_spawn o (d+1)
      else t in
    try_spawn o 0

  let shape2d : t -> (var * var) -> (float * float) list = fun o (v1, v2) ->
   let (v1_k, _) = key_of o v1 in
   let (v2_k, _) = key_of o v2 in
   (* Order the variables *)
   let (v1, v2, v1_k, v2_k) =
    if v1_k < v2_k then (v1, v2, v1_k, v2_k)
    else (v2, v1, v2_k, v1_k) in
   (* Get the rotated plane of these two variables. *)
   let plane = (v1_k, v2_k) in
   (* Retreive the bounds of the box. *)
   let (v1_lb, v1_ub, v2_lb, v2_ub) = (lb' o v1, ub' o v1, lb' o v2, ub' o v2) in
   (* Retreive the bounds of the rotated box. *)
   let k1 = (v1_k, plane) in
   let k2 = (v2_k, plane) in
   let (rv1_lb, rv1_ub, rv2_lb, rv2_ub) = (o.dbm.(lb_pos k1), o.dbm.(ub_pos k1), o.dbm.(lb_pos k2), o.dbm.(ub_pos k2)) in
   (* Compute the points of the bounding box. *)
   (* In the following order :
          a_b
         g/ \c
          | |
           ..
   *)
   let points =
    [(B.sub_up v2_ub rv2_ub, v2_ub);
     (B.sub_up rv1_ub v2_ub, v2_ub);
     (v1_ub, B.sub_up rv1_ub v1_ub);
     (v1_ub, B.sub_up v1_ub rv2_lb);
     (B.add_up rv2_lb v2_lb, v2_lb);
     (B.sub_up (B.neg rv1_lb) v2_lb, v2_lb);
     (v1_lb, B.sub_up (B.neg rv1_lb) v1_lb);
     (v1_lb, B.add_up rv2_ub v1_lb)] in
   List.map (fun (x,y) -> (B.to_float_up x, B.to_float_up y)) points

  (* split_on and shrink are relevant to pizza split.
     We do not implement yet this splitting strategy in the octagons. *)
  let split_on : t -> ctrs -> instance -> t list = fun _ _ _ ->
    Pervasives.failwith "BoxedOctagon: `split_on` is not implemented."
  let shrink : t -> Mpqf.t -> t = fun _ _ ->
    Pervasives.failwith "BoxedOctagon: function `shrink` unimplemented."
end

module BoxedOctagonReal
  (B:BOUND)
  (ItvDBM: IntervalViewDBM with
    type bound = B.t and
    type itv = Trigo.Make(Itv.Itv(B)).t)
= struct
  include BoxedOctagon(B)(ItvDBM)

  (* We add the variable regardless of its type (integer or real).
     It is an over-approximation to add integer. *)
  let add_var : t -> Csp.annot * Csp.var -> t = fun o (typ,var) ->
    add_var o typ var

  (* Check the consistency of the DBM, and update the cells in the diagonal to 0 (Miné, 2004). *)
  let is_consistent : t -> unit = fun o ->
    let n = o.dim in
    for i = 0 to (2*n-1) do
      if B.lt o.dbm.(matpos2 i i) B.zero then
        raise Bot.Bot_found
      else
        o.dbm.(matpos2 i i) <- B.zero
    done

  let is_failed : t -> bool = fun o ->
    try is_consistent o; false
    with Bot.Bot_found -> true
end

module BoxedOctagonF = struct
  include BoxedOctagonReal(Bound_float)(FloatIntervalDBM)
  module F = Bound_float

  (* Strong closure as appearing in (Miné, 2005) using a modified Floyd-Warshall algorithm. *)
  let strong_closure_mine : t -> t = fun o ->
    let o = copy o in
    let n = o.dim in
    for k = 0 to n-1 do
      strong_closure_k o k;
      strengthening o
    done;
    is_consistent o;
    meet_dbm_into_box o

  (* Throw Bot.Bot_found if an inconsistent abstract element is reached.
   * This filter procedure is currently very inefficient since we perform the closure (with Floyd-Warshall) every time we filter a constraint.
   * However, performing the better algorithm presented in (Pelleau, Chapter 5, 2012) requires we have all the constraints to filter at once (or an event system is implemented).
   *
   * (1) This algorithm first filters the constraint and its rotated versions individually.
   * (2) We merge the box in the DBM.
   * (3) Then we apply the Floyd Warshall algorithm.
   * (4) We merge the DBM in the box.
   *
   * Only rotated constraints with variables appearing in the rotated plane are executed.
   *
   * We could apply the step (1) to (4) until a fixpoint is reached but we leave the fixpoint computation to the propagation engine. *)
  let filter : t -> bconstraint -> t = fun o cons ->
    let o = copy o in
    let o = init_octagonalise o in
    if !Constant.debug > 1 then print_cons "filter: " cons;
    let o =
      match filter_octagonal o cons with
      | (o, true) -> if !Constant.debug > 1 then Printf.printf "octagonal\n"; o
      | (o, false) -> if !Constant.debug > 1 then Printf.printf "non octagonal\n"; filter_box o cons in
    strong_closure_mine o
end

module BoxedOctagonQ = struct
  include BoxedOctagonReal(Bound_mpqf)(RationalIntervalDBM)

  (* Strong closure as appearing in (Bagnara, 2009) using classical Floyd-Warshall algorithm followed by the strengthening procedure. *)
  let strong_closure_bagnara : t -> t = fun o ->
    let o = copy o in
    floyd_warshall o;
    is_consistent o;
    strengthening o;
    meet_dbm_into_box o

  (* Throw Bot.Bot_found if an inconsistent abstract element is reached.
   * This filter procedure is currently very inefficient since we perform the closure (with Floyd-Warshall) every time we filter a constraint.
   * However, performing the better algorithm presented in (Pelleau, Chapter 5, 2012) requires we have all the constraints to filter at once (or an event system is implemented).
   *
   * (1) This algorithm first filters the constraint and its rotated versions individually.
   * (2) We merge the box in the DBM.
   * (3) Then we apply the Floyd Warshall algorithm.
   * (4) We merge the DBM in the box.
   *
   * Only rotated constraints with variables appearing in the rotated plane are executed.
   *
   * We could apply the step (1) to (4) until a fixpoint is reached but we leave the fixpoint computation to the propagation engine. *)
  let filter : t -> bconstraint -> t = fun o cons ->
    let o = copy o in
    let o = init_octagonalise o in
    if !Constant.debug > 1 then print_cons "filter: " cons;
    let o =
      match filter_octagonal o cons with
      | (o, true) -> if !Constant.debug > 1 then Printf.printf "octagonal\n"; o
      | (o, false) -> if !Constant.debug > 1 then Printf.printf "non octagonal\n"; filter_box o cons in
    strong_closure_bagnara o
end

module BoxedOctagonI = struct
  include BoxedOctagon(Bound_int)(IntegerIntervalDBM)

  let support_only_integer_msg : string = "BoxedOctagonI: support only integer variables."

  (* Precondition: `typ` must be equal to `Int`. *)
  let add_var : t -> Csp.annot * Csp.var -> t = fun o (typ,var) ->
      (* This abstract domain only support real variables (for now). *)
    let check_type : Csp.annot -> unit = fun typ ->
      match typ with
      | Int -> ()
      | Real -> Pervasives.failwith support_only_integer_msg in
    check_type typ;
    add_var o Int var
end
