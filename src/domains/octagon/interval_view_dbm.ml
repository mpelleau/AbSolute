type plane = int * int
type dbm_key = int * plane

let if_rotated_else : dbm_key -> 'a -> 'a -> 'a = fun (v, (d1,d2)) then_b else_b ->
  if d1 <> d2 && (d1 = v || d2 = v) then then_b else else_b

(* Canonical plane *)
let cplane = (0,0)

let well_formed_plane (d1,d2) = ((d1 = 0 || d1 <> d2) && d1 <= d2)
let check_well_formed_plane plane = assert (well_formed_plane plane)

let lb_pos (v, (d1, d2)) =
  check_well_formed_plane (d1,d2);
  (* `v` correspond to the rotated plane along the axis `d1`. *)
  if v = d1 && d1 <> d2 then (2*d2),(2*d1+1)
  (* and here along the axis `d2`. *)
  else if v = d2 && d1 <> d2 then (2*d2),(2*d1)
  (* Non-rotated dimension, or canonical plane. *)
  else (v*2),(v*2+1)

(* Similar to `lb_pos`. *)
let ub_pos (v, (d1,d2)) =
  check_well_formed_plane (d1,d2);
  if v = d1 && d1 <> d2 then (2*d2+1),(2*d1)
  else if v = d2 && d1 <> d2 then (2*d2+1),(2*d1+1)
  else (v*2+1),(v*2)

module type IntervalViewDBM = sig
  type bound

  val dbm_to_lb : dbm_key -> bound -> bound
  val dbm_to_ub : dbm_key -> bound -> bound
  val lb_to_dbm : dbm_key -> bound -> bound
  val ub_to_dbm : dbm_key -> bound -> bound
end

module FloatIntervalDBM = struct
  module B = Bound_float
  module I = Trigo.Make(Itv.Itv(B))
  type bound = B.t

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

  let dbm_to_lb k v =
    let vi = I.of_float v in
    let divider = if_rotated_else k minus_sqrt2_it minus_two_it in
    lb_it (Bot.nobot (I.div vi divider))

  let dbm_to_ub k v =
    let vi = I.of_float v in
    let divider = if_rotated_else k sqrt2_it two_it in
    ub_it (Bot.nobot (I.div vi divider))

  let lb_to_dbm k v =
    let multiplier = if_rotated_else k minus_sqrt2_it minus_two_it in
    lb_it (I.mul (I.of_float v) multiplier)

  let ub_to_dbm k v =
    let multiplier = if_rotated_else k sqrt2_it two_it in
    ub_it (I.mul (I.of_float v) multiplier)
end

module RationalIntervalDBM = struct
  module B = Bound_rat
  module I = Trigo.Make(Itv.Itv(B))
  type bound = B.t

  let of_int : int -> bound = Bound_rat.of_int_up

  let sqrt2_it = I.of_rats (B.sqrt_down B.two) (B.sqrt_up B.two)
  let minus_sqrt2_it = I.neg sqrt2_it
  let lb_it i = let (l,_) = I.to_rational_range i in l
  let ub_it i = let (_,u) = I.to_rational_range i in u

  let dbm_to_lb k v =
    let vi = I.of_rat v in
    let in_plane = lb_it (Bot.nobot (I.div vi minus_sqrt2_it)) in
    if_rotated_else k in_plane (B.div_down v B.minus_two)

  let dbm_to_ub k v =
    let vi = I.of_rat v in
    let in_plane = ub_it (Bot.nobot (I.div vi sqrt2_it)) in
    if_rotated_else k in_plane (B.div_up v B.two)

  let lb_to_dbm k v =
    let in_plane = lb_it (I.mul (I.of_rat v) minus_sqrt2_it) in
    if_rotated_else k in_plane (B.mul_down v B.minus_two)

  let ub_to_dbm k v =
    let in_plane = ub_it (I.mul (I.of_rat v) sqrt2_it) in
    if_rotated_else k in_plane (B.mul_up v B.two)
end

module IntegerIntervalDBM = struct
  module B = Bound_int
  type bound = B.t

  let dbm_to_lb k v =
    B.of_rat_down (RationalIntervalDBM.dbm_to_lb k (RationalIntervalDBM.of_int v))
  let dbm_to_ub k v =
    B.of_rat_up (RationalIntervalDBM.dbm_to_ub k (RationalIntervalDBM.of_int v))
  let lb_to_dbm k v =
    B.of_rat_up (RationalIntervalDBM.lb_to_dbm k (RationalIntervalDBM.of_int v))
  let ub_to_dbm k v =
    B.of_rat_up (RationalIntervalDBM.ub_to_dbm k (RationalIntervalDBM.of_int v))
end