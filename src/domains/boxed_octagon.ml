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
  type base = int * int
  module R = Mapext.Make(struct
    type t=base
    let compare = compare end)
  type rotated_boxes = B.t R.t

  module Env = Tools.VarMap
  module REnv = Mapext.Make(struct
    type t=int
    let compare = compare end)
  type e = int Env.t
  type re = string REnv.t
  type t = {
    (* maps each variable name to its dimension `i` in the dbm. *)
    env : e;
    (* reversed mapping of `env`. *)
    renv : re;
    (* Number of variables *)
    dim: int;
    (* difference bound matrix *)
    dbm : bound array;
    (* Interval representation of the octagon (the canonical box). *)
    cbox : B.t;
    (* Interval representation of the canonical box in different rotated plan. *)
    rboxes : rotated_boxes
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

  let ub_idx : int -> base option -> int = fun dim base ->
    match base with
    | Some (d1, d2) when dim = d1 -> matpos2 (2*d2) (2*d1-1)
    | Some (d1, d2) when dim = d2 -> matpos2 (2*d2) (2*d1)
    | _ -> matpos2 (dim*2+1) (dim*2)

  let lb_idx : int -> base option -> int = fun dim base ->
    match base with
    | Some (d1, d2) when dim = d1 -> matpos2 (2*d2-1) (2*d1)
    | Some (d1, d2) when dim = d2 -> matpos2 (2*d2-1) (2*d1-1)
    | _ -> matpos2 (dim*2) (dim*2+1)

  let if_rotated_else : int -> base option -> 'a -> 'a -> 'a = fun dim base then_b else_b ->
    match base with
    | Some (d1, d2) when d1 = dim || d2 = dim -> then_b
    | _ -> else_b

  let ub : t -> int -> base option -> bound = fun o dim base ->
    let divider = if_rotated_else dim base (F.sqrt_down 2.) 2. in
    F.div_up o.dbm.(ub_idx dim base) divider

  let lb : t -> int -> base option -> bound = fun o dim base ->
    let divider = if_rotated_else dim base (F.neg (F.sqrt_down 2.)) (-2.) in
    F.div_down o.dbm.(lb_idx dim base) divider

  let set_ub : t -> int -> base option -> bound -> unit = fun o dim base v ->
    let multiplier = if_rotated_else dim base (F.sqrt_up 2.) 2. in
    o.dbm.(ub_idx dim base) <- F.mul_up v multiplier

  let set_lb : t -> int -> base option -> bound -> unit = fun o dim base v ->
    let multiplier = if_rotated_else dim base (F.neg (F.sqrt_up 2.)) (-2.) in
    o.dbm.(ub_idx dim base) <- F.mul_down v multiplier

  (* Add a box rotated in a random plan (d1, d2) such that d2 > d1. *)
  let random_octagonalisation : t -> t = fun o ->
    Random.init 0;
    let d1 = (Random.int (o.dim - 1)) in
    let d2 = (Random.int (o.dim - d1 - 1)) + d1 + 1 in
    let rboxes = R.add (d1,d2) o.cbox o.rboxes in
    { o with rboxes=rboxes }

  (* This function must be called after all the unconstrained variables have been added,
   * and before any constraint is filtered. *)
  let init_octagonalise : t -> t = fun o ->
    if o.dim >= 2 && R.is_empty o.rboxes then
      match !octagonalisation with
        Random -> random_octagonalisation o
      | _ -> Pervasives.failwith "BoxedOctagon: this split is not implemented; only LargestFirst (lf) is currently implemented."
    else o

  (* returns an empty element *)
  let empty : t = { env=Env.empty; renv=REnv.empty; dim=0; dbm=[||]; cbox=B.empty; rboxes=R.empty }

  let copy : t -> t = fun o ->
    { env = o.env; renv=o.renv; dim=o.dim; dbm=Array.copy o.dbm; cbox=o.cbox; rboxes=o.rboxes }

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
    let renv' = REnv.add dim' var o.renv in
    (* allocate in the matrix two rows of size 2*dim' with a top bound. *)
    let top = F.inf in
    let row = Array.make (dim'*2*2) top in
    let dbm' = Array.append o.dbm row in
    let cbox' = B.add_var o.cbox (typ, var) in
    { env=env'; renv=renv'; dim=dim'; dbm=dbm'; cbox=cbox'; rboxes=o.rboxes }

  let var_bounds_from_dim : t -> int -> base option -> (Mpqf.t * Mpqf.t) = fun o dim base ->
    (F.to_rat (lb o dim base), F.to_rat (ub o dim base))

  (* returns the bounds of a variable *)
  let var_bounds : t -> Csp.var -> (Mpqf.t * Mpqf.t) = fun o var ->
    let dim = Env.find var o.env in
    var_bounds_from_dim o dim None

  (* returns the bound variables *)
  let bound_vars : t -> Csp.csts = fun o ->
    B.bound_vars o.cbox

  (* removes an unconstrained variable from the environnement *)
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

  let meet_dbm_into_box : t -> base option -> B.t -> B.t = fun o base box ->
    let filter_var = fun k dim box ->
      let (l,u) = var_bounds_from_dim o dim base in
      let constraints = from_cst_to_expr (k, (l, u)) in
      List.fold_left B.filter box constraints in
    Env.fold filter_var o.env box

  let meet_dbm_into_boxes : t -> t = fun o ->
    let cbox = meet_dbm_into_box o None o.cbox in
    let meet_rbox = fun k v -> meet_dbm_into_box o (Some k) v in
    let rboxes = R.mapi meet_rbox o.rboxes in
    { o with cbox=cbox; rboxes=rboxes }

  let meet_box_into_dbm : t -> base option -> B.t -> t = fun o base box ->
    let update_cell = fun k dim ->
      let (l, u) = B.float_bounds box k in
      set_lb o dim base l;
      set_ub o dim base u;
    in
    Env.iter update_cell o.env;
    o

  (* Check the consistency of the DBM. *)
  let float_consistent : t -> unit = fun o ->
    let n = o.dim in
    for i = 1 to (2*n) do
      let idx = matpos2 (i-1) (i-1) in
      if o.dbm.(idx) < 0. then
        raise Bot.Bot_found
      else
        o.dbm.(idx) <- 0.
    done

  (* The part of the Floyd-Washall algorithm for a given 'k'. *)
  let strong_closure_k : t -> int -> unit = fun o k ->
    let m = fun i j -> o.dbm.(matpos2 (i-1) (j-1)) in
    let n = o.dim in
    for i = 1 to (2*n) do
      for j = 1 to (2*n) do
        let v = List.fold_left F.min F.inf [
          F.add_up (m i (2*k)) (m (2*k) j) ;
          F.add_up (m i (2*k-1)) (m (2*k-1) j) ;
          F.add_up (F.add_up (m i (2*k-1)) (m (2*k -1) (2*k))) (m (2*k) j) ;
          F.add_up (F.add_up (m i (2*k)) (m (2*k) (2*k-1))) (m (2*k-1) j) ] in
        if v < m i j then
          o.dbm.(matpos2 (i-1) (j-1)) <- v
      done
    done

  (* Strengthening propagates the octagonal constraints in the DBM. *)
  let strengthening : t -> unit = fun o ->
    let m = fun i j -> o.dbm.(matpos2 (i-1) (j-1)) in
    let n = o.dim in
    for i = 1 to (2*n) do
      for j = 1 to (2*n) do
        let i' = if i mod 2 = 0 then i - 1 else i + 1 in
        let j' = if j mod 2 = 0 then j - 1 else j + 1 in
        if (F.add_up (m i i')  (m j' j)) < (m i j) then
          o.dbm.(matpos (i-1) (j-1)) <- (F.add_up (m i i') (m j' j))
      done
    done

  (* Strong closure as appearing in (Miné, 2005) using a modified Floyd-Warshall algorithm. *)
  let strong_closure_mine : t -> t = fun o ->
    let o = copy o in
    let n = o.dim in
    for k = 1 to n do
      strong_closure_k o k;
      strengthening o
    done;
    float_consistent o;
    o

  (* Strong closure as appearing in (Bagnara, 2009) using Floyd-Warshall algorithm followed by the strengthening procedure. *)
  let strong_closure_bagnara : t -> t = fun o ->
    let o = copy o in
    let n = o.dim in
    for k = 1 to n do
      strong_closure_k o k;
    done;
    float_consistent o;
    strengthening o;
    o

  (* pruning *)
  let prune : t -> t -> t list * t
    = fun o o' -> Pervasives.failwith "BoxedOctagon: function `prune` unimplemented."

  (* Largest first split: select the biggest variable in the canonical box and split on its middle value.
   * We rely on the split of Box. *)
  let split_lf : t -> t list = fun o ->
    let create_node = fun cbox ->
      let o' = copy o in
      let o' = { o' with cbox=cbox } in
      meet_box_into_dbm o' None o'.cbox in
    let cboxes = B.split o.cbox in
    List.map create_node cboxes

  (* splits an abstract element *)
  let split : t -> t list = fun o ->
    match !boct_split with
    | LargestFirst -> split_lf o
    | _ -> Pervasives.failwith "BoxedOctagon: this split is not implemented; only LargestFirst (lf) is currently implemented."

  let rotate_var : t -> (var * var) -> (expr * expr) = fun o (v1,v2) ->
    (* Symbolic representation of the square root of 2.
       If it is computed right away, we do not know in what direction we should round the square root.
     *)
    let sqrt2 = Binary (POW, (Cst (Mpqf.of_int 2, Real)), (Cst (Mpqf.of_frac 1 2, Real))) in
    let left = Binary (DIV, Var v1, sqrt2) in
    let right = Binary (DIV, Var v2, sqrt2) in
    let rv1 = Binary (SUB, left, right) in
    let rv2 = Binary (ADD, left, right) in
    (rv1, rv2)

  let rotate_constraint : t -> bconstraint -> base -> bconstraint option = fun o (e1,op,e2) (d1,d2) ->
    let v1 = REnv.find d1 o.renv in
    let v2 = REnv.find d2 o.renv in
    let (rv1, rv2) = rotate_var o (v1, v2) in
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

  let filter_in_base : bconstraint -> base option -> B.t -> t -> t = fun cons base box o ->
    match base with
    | None ->
        let o' = { o with cbox=B.filter box cons } in
        meet_box_into_dbm o' base o'.cbox
    | Some base' ->
        match rotate_constraint o cons base' with
        | None -> o
        | Some rcons ->
            let box' = B.filter box rcons in
            let rboxes = R.add base' box' o.rboxes in
            meet_box_into_dbm { o with rboxes=rboxes } base box'

  (* Throw Bot.Bot_found if an inconsistent abstract element is reached.
   * This filter procedure is currently very inefficient since we perform the closure (with Floyd-Warshall) every time we call a filtering on a constraint.
   * However, performing the better algorithm presented in (Pelleau, Chapter 5, 2012) requires we have all the constraints to filter at once (or an event system in place).
   *
   * (1) This filter algorithm first filter the constraint and its rotated versions individually and merge their results in the DBM.
   * (2) Then we apply the Floyd Warshall algorithm.
   * (3) We merge the DBM with the boxes.
   *
   * Only rotated constraints with variables appearing in the rotated base are executed. *)
  let filter : t -> bconstraint -> t = fun o cons ->
    let filter_rotated base = filter_in_base cons (Some base) in
    o |>
    init_octagonalise |>
    filter_in_base cons None o.cbox |>
    R.fold filter_rotated o.rboxes |>
    strong_closure_mine |>
    meet_dbm_into_boxes

  (* We delegate this evaluation to the canonical box.
   * Possible improvement: obtain a better approximation by turning the expression and forward_eval it in different base. *)
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

  let range i j =
    let rec aux n acc =
      if n < i then acc else aux (n-1) (n :: acc)
    in aux j []

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
      let (v,i) = B.max_range b in
      I.float_size i in
    let p = R.fold (fun _ b -> min (box_max_range b)) o.rboxes (box_max_range o.cbox) in
    p <= !Constant.precision

  let is_failed : t -> bool = fun o ->
    try float_consistent o; false
    with Bot.Bot_found -> true

  let is_empty : t -> bool
    = fun o -> Env.is_empty o.env || is_failed o

end
