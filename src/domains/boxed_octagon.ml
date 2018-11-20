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

module BoxedOctagon = struct
  module F = Bound_float
  type bound = F.t
  type m = bound array
  module I = Trigo.Make(Itv.ItvF)
  module B = Box(I)
  type plane = int * int
  (* An element in the DBM is uniquely identified by its plane and dimension. *)
  type key = int * plane

  module Env = Tools.VarMap
  module REnv = Mapext.Make(struct
    type t=key
    let compare = compare end)
  type e = key Env.t
  type re = string REnv.t
  (* Invariants:
      * For all key, we have: `key = REnv.find renv (Env.find env key)`
      * `B.length box = (dim + 2 * (List.length planes))`
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
    box : B.t;
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
    ((d1 == 0 || d1 <> d2) && d1 <= d2)

  (* We represent the canonical plane (where d1==d2) only with (0,0). *)
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

  (* If the variable `v` is rotated in the plane `(d1,d2)` then returns `then_b`, otherwise `else_b`. *)
  let if_rotated_else : key -> 'a -> 'a -> 'a = fun (v, (d1,d2)) then_b else_b ->
    if d1 <> d2 && (d1 = v || d2 = v) then then_b else else_b

  (* returns an empty element *)
  let empty : t = { env=Env.empty; renv=REnv.empty; dim=0; dbm=[||]; box=B.empty; planes=[] }

  (* This function copy the imperative data structure of the octagon to create a fresh one.
     It is necessary for the search algorithm which backtracks since `dbm` is mutable (and thus not automatically copied). *)
  let copy : t -> t = fun o ->
    { env = o.env; renv=o.renv; dim=o.dim; dbm=Array.copy o.dbm; box=o.box; planes=o.planes }

  let length : t -> int = fun o -> o.dim
  let is_empty : t -> bool = fun o -> (length o) = 0
  let dbm_length : t -> int = fun o -> Array.length o.dbm
  let planes_length : t -> int = fun o -> List.length o.planes

  let support_only_real_msg : string = "BoxedOctagon: only support real variables."

  (* Allocate in the matrix two rows of size 2*dim' with an infinite bound. *)
  let add_var_in_dbm : t -> t = fun o ->
    let o = copy o in
    (* increase the dimension. *)
    let dim' = o.dim + 1 in
    let top = F.inf in
    let row = Array.make (dim'*2*2) top in
    let dbm' = Array.append o.dbm row in
    { o with dim=dim'; dbm=dbm' }

  (* Register the `key` of the variable `var` in the environment. *)
  let add_var_in_box : t -> Csp.var -> key -> t = fun o var key ->
    let env' = Env.add var key o.env in
    let renv' = REnv.add key var o.renv in
    let box' = B.add_var o.box (Real, var) in
    { o with env=env'; renv=renv'; box=box' }


  (* Adds an unconstrained variable to the octagon.
     Precondition: `typ` must be equal to `Real`.
  *)
  let add_var : t -> Csp.annot * Csp.var -> t = fun o (typ,var) ->
    (* This abstract domain only support real variables (for now). *)
    let check_type : Csp.annot -> unit = fun typ ->
      match typ with
      | Int -> Pervasives.failwith support_only_real_msg
      | Real -> () in
    check_type typ;
    let o = add_var_in_dbm o in
    let key = (o.dim-1, cplane) in
    add_var_in_box o var key

  (* Rules for coping with rounding when transferring from DBM to BOX:
      * From BOX to DBM: every number is rounded UP because these numbers only decrease during the Floyd Warshall algorithm.
      * From DBM to BOX: the number is rounded DOWN for lower bound and UP for upper bound.

     To simplify the treatment (and improve soundness), we use interval arithmetic: (sqrt 2) is interpreted as the interval [sqrt_down 2, sqrt_up 2].
     Further operations are performed on this interval, and we chose the lower or upper bound at the end depending on what we need.
  *)

  let two_it = I.of_float (F.two)
  let minus_two_it = I.neg two_it
  let sqrt2_it = I.of_floats (F.sqrt_down F.two) (F.sqrt_up F.two)
  let minus_sqrt2_it = I.neg sqrt2_it
  let lb_it i = let (l,_) = I.to_float_range i in l
  let ub_it i = let (_,u) = I.to_float_range i in u

  (* Lower bound of the variable at key `k`.
   The value is computed directly from the DBM with care on rounding. *)
  let lb : t -> key -> bound = fun o k ->
    let v = I.of_float o.dbm.(lb_pos k) in
    let divider = if_rotated_else k minus_sqrt2_it minus_two_it in
    lb_it (Bot.nobot (I.div v divider))

  (* Same as `lb` but for the upper bound. *)
  let ub : t -> key -> bound = fun o k ->
    let v = I.of_float o.dbm.(ub_pos k) in
    let divider = if_rotated_else k sqrt2_it two_it in
    ub_it (Bot.nobot (I.div v divider))

  (* Monotonic write: We set a value in the DBM only if the current one is larger. *)
  let set : t -> int -> bound -> unit = fun o pos v ->
    if o.dbm.(pos) > v then
      o.dbm.(pos) <- v

  (* Set the lower bound of the variable `k` in the DBM.
     The value `v` is the lower bound of the value at key `k`, and is processed to fit in the DBM. *)
  let set_lb : t -> key -> bound -> unit = fun o k v ->
    let pos = (lb_pos k) in
    let multiplier = if_rotated_else k minus_sqrt2_it minus_two_it in
    set o pos (lb_it (I.mul (I.of_float v) multiplier))

  (* Same as `set_lb` but for the upper bound. *)
  let set_ub : t -> key -> bound -> unit = fun o k v ->
    let pos = (ub_pos k) in
    let multiplier = if_rotated_else k sqrt2_it two_it in
    set o pos (ub_it (I.mul (I.of_float v) multiplier))

  let internal_suffix = "_BoxedOctagon_internal"
  let internal_name canonical_name d1 d2 =
    canonical_name ^ "_in_" ^ (string_of_int d1) ^ "_" ^ (string_of_int d2) ^ internal_suffix

  let is_canonical_name : t -> var -> bool = fun o name ->
    let (_,plane) = Env.find name o.env in
    cplane == plane

  let var_name : t -> key -> var = fun o (v, (d1,d2)) ->
    try
    let canonical_name = REnv.find (v, cplane) o.renv in
    if (d1,d2) == cplane then
      canonical_name
    else
      internal_name canonical_name d1 d2
    with Not_found -> failwith (string_of_int v)

  let check_add_plane : t -> plane -> unit = fun o (d1,d2) ->
    check_well_formed_plane (d1,d2);
    assert (o.dim > d1 && o.dim > d2 && d1 <> d2);
    assert (not (List.mem (d1,d2) o.planes))

  let add_plane : t -> plane -> t = fun o (d1,d2) ->
    let plane = (d1,d2) in
    check_add_plane o plane;
    let add o key =
      let name = var_name o key in
      add_var_in_box o name key in
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
    if o.dim >= 2 && (List.length o.planes) = 0 then
      match !octagonalisation with
        Random -> random_octagonalisation o
      | _ -> Pervasives.failwith "BoxedOctagon: this split is not implemented; only LargestFirst (lf) is currently implemented."
    else o

  let all_vars : t -> (Csp.annot * Csp.var) list = fun o ->
    Env.fold (fun v _ acc -> (Real, v)::acc) o.env []

  (* Returns the variables registered in the octagon `o`.
     It does not return the variables created by the octagon (those on planes).
  *)
  let vars : t -> (Csp.annot * Csp.var) list = fun o ->
    List.filter
      (fun (_, v) -> is_canonical_name o v)
      (all_vars o)

  (* Returns the bounds of the variable `k` in `plane` (as currently set in the DBM). *)
  let bounds_of_var : t -> key -> (Mpqf.t * Mpqf.t) = fun o key ->
    (F.to_rat (lb o key), F.to_rat (ub o key))

  (* Returns the bounds of the variable `var`.
     See also `bounds_of_var`.
  *)
  let var_bounds : t -> Csp.var -> (Mpqf.t * Mpqf.t) = fun o var ->
    let key = Env.find var o.env in
    bounds_of_var o key

  (* Returns the variables instantiated (the lower bound is exactly equal to the upper bound).
     It does not return the variables instantiated in a plane.
  *)
  let bound_vars : t -> Csp.csts = fun o ->
    List.filter
      (fun (v, _) -> is_canonical_name o v)
      (B.bound_vars o.box)

(*

  (* Removes an unconstrained variable from the environment. *)
  let rem_var : t -> Csp.var -> t = fun o _ -> o

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
  (* We only keep the intersection of the rotated boxes. *)
  let join_rboxes : rotated_boxes -> rotated_boxes -> rotated_boxes = fun a b ->
    let c: rotated_boxes = R.filter (fun k _ -> R.exists (fun k' _ -> k = k') b) a in
    R.mapi (fun k -> B.join (R.find k b)) c

  let join : t -> t -> t = fun o o' ->
    let (res, rest) =
      if Array.length o.dbm >= Array.length o'.dbm then
        (o, o')
      else
        (o', o)
    in
    let res = copy res in
    let res = { res with
      cbox=(B.join res.cbox rest.cbox);
      rboxes=(join_rboxes res.rboxes rest.rboxes) } in
    check_same_env res rest;
    let merge = fun idx x -> res.dbm.(idx) <- (F.max x res.dbm.(idx)) in
    Array.iteri merge rest.dbm;
    res

  let meet_dbm_into_box : t -> plane -> B.t -> B.t = fun o plane box ->
    let filter_var = fun var_name k box ->
      let (l,u) = bounds_of_var o k plane in
      let constraints = from_cst_to_expr (var_name, (l, u)) in
      List.fold_left B.filter box constraints in
    Env.fold filter_var o.env box

  let meet_dbm_into_boxes : t -> t = fun o ->
    let cbox = meet_dbm_into_box o cplane o.cbox in
    let meet_rbox = meet_dbm_into_box o in
    let rboxes = R.mapi meet_rbox o.rboxes in
    { o with cbox=cbox; rboxes=rboxes }

  let meet_box_into_dbm : t -> plane -> B.t -> t = fun o plane box ->
    let update_cell = fun var_name k ->
      let (l, u) = B.float_bounds box var_name in
      set_lb o k plane l;
      set_ub o k plane u;
    in
    Env.iter update_cell o.env;
    o

  (* We raise `Bot_found` if there is a negative cycle (v < 0).
     Otherwise we set the value `v` in the DBM[i,j] if it improves the current value.
     Note: To avoid overflow, we must check negative cycle at every loop (Hougardy, 2010). *)
  let update_cell : t -> int -> int -> bound -> unit = fun o i j v ->
    let idx = matpos2 i j in
    if v < o.dbm.(idx) then
      if v < 0. then
        raise Bot.Bot_found
      else
        o.dbm.(idx) <- v
        (* TODO: reschedule the corresponding propagators. *)
    else ()

  (* Check the consistency of the DBM, and update the cell to 0.
     Note: Since we always update the DBM with `update_cell`, we are actually sure that the matrix is consistent.
     This method just set the diagonal elements to 0.
     We keep this name to match the algorithm in (Bagnara, 2009). *)
  let float_consistent : t -> unit = fun o ->
    let n = o.dim in
    for i = 0 to (2*n-1) do
      o.dbm.(matpos2 i i) <- 0.
    done

  (* The part of the modified Floyd-Warshall algorithm for a given 'k'. *)
  let strong_closure_k : t -> int -> unit = fun o k ->
    let m = fun i j -> o.dbm.(matpos2 i j) in
    let n = o.dim in
    for i = 0 to (2*n-1) do
      for j = 0 to (2*n-1) do
        let v = List.fold_left F.min (m i j) [
          F.add_up (m i (2*k+1)) (m (2*k+1) j) ;
          F.add_up (m i (2*k)) (m (2*k) j) ;
          F.add_up (F.add_up (m i (2*k)) (m (2*k) (2*k+1))) (m (2*k+1) j) ;
          F.add_up (F.add_up (m i (2*k+1)) (m (2*k+1) (2*k))) (m (2*k) j) ] in
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
        let v = (F.div_up (F.add_up (m i i') (m j' j)) 2.) in
        update_cell o i j v
      done
    done

  (* Strong closure as appearing in (Miné, 2005) using a modified Floyd-Warshall algorithm. *)
  let strong_closure_mine : t -> t = fun o ->
    let o = copy o in
    let n = o.dim in
    for k = 0 to n-1 do
      strong_closure_k o k;
      strengthening o
    done;
    float_consistent o;
    o

  (* The Floyd-Warshall algorithm. *)
  let floyd_warshall : t -> unit = fun o ->
    let m = fun i j -> o.dbm.(matpos2 i j) in
    let n = o.dim in
    for k = 0 to (2*n-1) do
      for i = 0 to (2*n-1) do
        for j = 0 to (2*n-1) do
          let v = F.min (m i j) (F.add_up (m i k) (m k j)) in
          update_cell o i j v
        done
      done
    done

  (* Strong closure as appearing in (Bagnara, 2009) using classical Floyd-Warshall algorithm followed by the strengthening procedure. *)
  let strong_closure_bagnara : t -> t = fun o ->
    let o = copy o in
    floyd_warshall o;
    float_consistent o;
    strengthening o;
    o

  (* pruning *)
  let prune : t -> t -> t list * t
    = fun _ _ -> Pervasives.failwith "BoxedOctagon: function `prune` unimplemented."

  (* Largest first split: select the biggest variable in the canonical box and split on its middle value.
   * We rely on the split of Box. *)
  let split_lf : t -> ctrs -> t list = fun o c ->
    let create_node = fun cbox ->
      let o' = copy o in
      let o' = { o' with cbox=cbox } in
      meet_box_into_dbm o' cplane o'.cbox in
    let cboxes = B.split o.cbox c in
    List.map create_node cboxes

  (* splits an abstract element *)
  let split : t -> ctrs -> t list = fun o c ->
    match !boct_split with
    | LargestFirst -> split_lf o c
    | _ -> Pervasives.failwith "BoxedOctagon: this split is not implemented; only LargestFirst (lf) is currently implemented."

  let split_on (o:t) (_:ctrs) (_:instance) : t list = [o]

  let shrink (o:t) (_:Mpqf.t) : t = o

  let rotate_var : (var * var) -> (expr * expr) = fun (v1,v2) ->
    (* Symbolic representation of the square root of 2.
       If it is computed right away, we do not know in what direction we should round the square root.
     *)
    let sqrt2 = Binary (POW, (Cst (Mpqf.of_int 2, Real)), (Cst (Mpqf.of_frac 1 2, Real))) in
    let left = Binary (DIV, Var v1, sqrt2) in
    let right = Binary (DIV, Var v2, sqrt2) in
    let rv1 = Binary (SUB, left, right) in
    let rv2 = Binary (ADD, left, right) in
    (rv1, rv2)

  let rotate_constraint : t -> bconstraint -> plane -> bconstraint option = fun o (e1,op,e2) (d1,d2) ->
    let v1 = REnv.find d1 o.renv in
    let v2 = REnv.find d2 o.renv in
    let (rv1, rv2) = rotate_var (v1, v2) in
    let found_var = ref false in
    let replace = fun v ->
      if v = v1 || v = v2 then begin
        found_var := true;
        if v = v1 then rv1 else rv2 end
      else Var v
    in
    let e1' = replace_var_in_expr replace e1 in
    let e2' = replace_var_in_expr replace e2 in
    if !found_var then Some (e1', op, e2') else None

  let filter_in_plane : bconstraint -> plane -> B.t -> t -> t = fun cons plane box o ->
    if plane = cplane then
      let o' = { o with cbox=B.filter box cons } in
      meet_box_into_dbm o' plane o'.cbox
    else
      match rotate_constraint o cons plane with
      | None -> o
      | Some rcons ->
          let box' = B.filter box rcons in
          let rboxes = R.add plane box' o.rboxes in
          meet_box_into_dbm { o with rboxes=rboxes } plane box'

  (* Throw Bot.Bot_found if an inconsistent abstract element is reached.
   * This filter procedure is currently very inefficient since we perform the closure (with Floyd-Warshall) every time we call a filtering on a constraint.
   * However, performing the better algorithm presented in (Pelleau, Chapter 5, 2012) requires we have all the constraints to filter at once (or an event system in place).
   *
   * (1) This algorithm first filters the constraint and its rotated versions individually and merge their results in the DBM.
   * (2) Then we apply the Floyd Warshall algorithm.
   * (3) We merge the DBM with the boxes.
   *
   * Only rotated constraints with variables appearing in the rotated plane are executed. *)
  let filter : t -> bconstraint -> t = fun o cons ->
    let filter_rotated plane = filter_in_plane cons plane in
    o |>
    init_octagonalise |>
    filter_in_plane cons cplane o.cbox |>
    R.fold filter_rotated o.rboxes |>
    strong_closure_mine |>
    meet_dbm_into_boxes

  (* We delegate this evaluation to the canonical box.
   * Possible improvement: obtain a better approximation by turning the expression and forward_eval it in different plane. *)
  let forward_eval : t -> Csp.expr -> (Mpqf.t * Mpqf.t) = fun o e ->
    B.forward_eval o.cbox e

  let create_var_from_cell : t -> int -> expr = fun o i ->
    let x = REnv.find (i/2) o.renv in
    if i mod 2 = 0 then
      Var x
    else
      Unary (NEG, Var x)

  let dbm_cell_to_bexpr : t -> int -> bconstraint list -> int -> bconstraint list = fun o i res j ->
    let idx = matpos i j in
    (* We do not generate the constraint where the coefficient is infinite. *)
    if o.dbm.(idx) <> F.inf then
      (* x - y <= c *)
      let x = create_var_from_cell o i in
      let y = create_var_from_cell o j in
      let c = Cst ((Mpqf.of_float o.dbm.(idx)), Real) in
      let left = simplify_fp (Binary (SUB, x, y)) in
      res@[(left,LEQ,c)]
    else
      res

  (* transforms an abstract element in constraints *)
  let to_bexpr : t -> bconstraint list = fun o ->
    let n = o.dim in
    let aux res i =
      List.fold_left (dbm_cell_to_bexpr o i) res (range 0 ((i lxor 1)*2-1))
    in
    List.fold_left aux [] (range 0 (2*n-1))

  (* check if a constraint is suited for this abstract domain *)
  let is_representable : Csp.bexpr -> answer =
    fun _ -> Adcp_sig.Yes

  let print : Format.formatter -> t -> unit = fun fmt o ->
    B.print fmt o.cbox

  (* concretization function. we call it a spawner.
     useful to do tests, and to reuse the results.
     values are generated randomly *)
  let spawn : t -> Csp.instance = fun o ->
    B.spawn o.cbox

  (* check if an abstract element is an abstractin of an instance *)
  let is_abstraction : t -> Csp.instance -> bool = fun o vars ->
    B.is_abstraction o.cbox vars

  (*** PREDICATES ***)
  let volume : t -> float = fun o ->
    B.volume o.cbox

  (* tests if an abstract element is too small to be cut *)
  let is_small : t -> bool = fun o ->
    let box_max_range b =
      let (_,i) = B.max_range b in
      I.float_size i in
    let p = R.fold (fun _ b -> min (box_max_range b)) o.rboxes (box_max_range o.cbox) in
    p <= !Constant.precision

  let is_failed : t -> bool = fun o ->
    try float_consistent o; false
    with Bot.Bot_found -> true *)
end
