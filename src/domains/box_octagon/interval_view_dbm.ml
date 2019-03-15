(*
open Dbm

let is_rotated (x,y) =


 (v, (d1,d2)) = d1 <> d2 && (d1 = v || d2 = v)

let if_rotated_else : dbm_key -> 'a -> 'a -> 'a = fun k then_b else_b ->
  if is_rotated k then then_b else else_b

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

module type Interval_view_sig = functor (B: Bound_sig.BOUND) ->
sig
  module B: Bound_sig.BOUND
  type bound = B.t

  val dbm_to_lb : dbm_key -> bound -> bound
  val dbm_to_ub : dbm_key -> bound -> bound
  val lb_to_dbm : dbm_key -> bound -> bound
  val ub_to_dbm : dbm_key -> bound -> bound
end with module B=B

(* Rules for coping with rounding when transferring from DBM to BOX:
  * From BOX to DBM: every number is rounded UP because these numbers only decrease during the Floyd Warshall algorithm.
  * From DBM to BOX: the number is rounded DOWN for lower bound and UP for upper bound.

  To simplify the treatment (and improve soundness), we use interval arithmetic: (sqrt 2) is interpreted as the interval [sqrt_down 2, sqrt_up 2].
  Further operations are performed on this interval, and we chose the lower or upper bound at the end depending on what we need. *)
module Interval_view(B: Bound_sig.BOUND) =
struct
  module B=B
  module R=Bound_rat
  type bound = B.t

  module I = Trigo.Make(Itv.Itv(R))

  let wrap of_rat f' k v = of_rat (f' k (B.to_rat v))

  let sqrt2_it = I.of_rats (R.sqrt_down R.two) (R.sqrt_up R.two)
  let minus_sqrt2_it = I.neg sqrt2_it

  let dbm_to_lb = wrap B.of_rat_down (fun k v ->
    if is_rotated k then
      I.lb (Bot.nobot (I.div (I.of_rat v) minus_sqrt2_it))
    else
      R.div v R.minus_two)

  let dbm_to_ub = wrap B.of_rat_up (fun k v ->
    if is_rotated k then
      I.ub (Bot.nobot (I.div (I.of_rat v) sqrt2_it))
    else
      R.div v R.two)

  let lb_to_dbm = wrap B.of_rat_up (fun k v ->
    if is_rotated k then
      I.ub (I.mul (I.of_rat v) minus_sqrt2_it)
    else
      R.mul v R.minus_two)

  let ub_to_dbm = wrap B.of_rat_up (fun k v ->
    if is_rotated k then
      I.ub (I.mul (I.of_rat v) sqrt2_it)
    else
      R.mul v R.two)
end
*)