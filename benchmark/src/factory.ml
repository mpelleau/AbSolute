open Bench_desc_t
open Bound_sig
open Itv_sig
open Rcpsp

let make_bound : bound -> (module BOUND) = function
| `Rational -> (module Bound_rat)
| `Integer -> (module Bound_int)
| `Float -> (module Bound_float)

let make_interval : interval -> (module ITV) = function
  `Interval(b) ->
    let (module B : BOUND) = make_bound b in
    (module Itv.Itv(B))
| `IntervalOpenClose(b) ->
    let (module B : BOUND) = make_bound b in
    (module Newitv.Make(B))
| `IntervalMixFloatInteger -> (module Itv_mix)

let make_abstract_domain : abstract_domain -> (module Adcp_sig.AbstractCP) = function
  `Box(i) ->
    let (module I: ITV) = make_interval i in
    (module Abstract_box.Box(I))
| `BoxedOctagon(b) -> Boxed_octagon.(
    match b with
    | `Rational -> (module BoxedOctagonQ)
    | `Integer -> (module BoxedOctagonZ)
    | `Float -> (module BoxedOctagonF))

(* Precondition: Sanity checks on the file path are supposed to be already done, otherwise it can throw I/O related exceptions.
The files from PSPlib are also supposed to be well-formatted. *)
let make_rcpsp config problem_path =
  let rcpsp =
    match config.problem_kind with
    | `PSPlib -> Sm_format.read_sm_file problem_path
    | `Patterson -> Patterson.read_patterson_file problem_path
    | `ProGenMax -> Pro_gen_max.read_pro_gen_file problem_path in
  rcpsp
