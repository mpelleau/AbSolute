open Apron
open Utils
open ADCP
open Mpqf

let zero = Mpqf.of_int 0
let pi = Mpqf.of_float 3.14159265358979323846
let two_pi = Mpqf.of_float 6.28318530717958647693
let pi_half = Mpqf.of_float 1.57079632679489661923
let pi_quarter = Mpqf.of_float 0.78539816339744830962

let minus_one = Mpqf.of_int (-1)
let plus_one = Mpqf.of_int 1

let minus_plus_one = Interval.of_mpqf minus_one plus_one

(** Adds a value to an interval *)
let add itv mpqf =
  let inf = scalar_to_mpqf itv.Interval.inf
  and sup = scalar_to_mpqf itv.Interval.sup in
  Interval.of_mpqf (Mpqf.add inf mpqf) (Mpqf.add sup mpqf)

(** Returns the quadrant in which the real value is.
  * Value must be between 0 and 2 pi *)
let quadrant value =
  if (Mpqf.cmp value pi_half) <= 0 then 1
  else if (Mpqf.cmp value pi) <= 0 then 2
  else if (Mpqf.cmp value (Mpqf.add pi pi_half)) <= 0 then 3
  else 4

(** The value is scaled to the range [0, 2pi] *)
let scale_to_two_pi sca =
  let q = floor (Mpqf.to_float (Mpqf.div sca two_pi)) in
  if q <= 0. then Mpqf.add two_pi (Mpqf.sub sca (Mpqf.mul pi (Mpqf.of_float q)))
  else Mpqf.sub sca (Mpqf.mul two_pi (Mpqf.of_float q))

(** The interval is scaled to the range [0, 2pi] *)
let scale_to_two_pi_itv itv =
  let inf = scalar_to_mpqf itv.Interval.inf
  and sup = scalar_to_mpqf itv.Interval.sup in
  if (Mpqf.cmp inf zero) >= 0 && (Mpqf.cmp sup two_pi) < 0 then itv
  else Interval.of_mpqf (scale_to_two_pi inf) (scale_to_two_pi sup)

(** Evaluate the sin function over an interval *)
let evaluate_sin itv =
  let diam = diam_interval itv in
  if (Mpqf.cmp diam two_pi) >= 0 then
    minus_plus_one
  else
    let itv' = scale_to_two_pi_itv itv in
    let inf = scalar_to_mpqf itv'.Interval.inf
    and sup = scalar_to_mpqf itv'.Interval.sup
    and diam = diam_interval itv' in
    let q_inf = quadrant inf
    and q_sup = quadrant sup in
    if q_inf = q_sup && (Mpqf.cmp diam pi) >= 0 then
      minus_plus_one
    else
      let inf_flt = Mpqf.to_float inf
      and sup_flt = Mpqf.to_float sup in
      let sin_inf = sin inf_flt
      and sin_sup = sin sup_flt in
      (* If the rounding mode can be changed then the inf bound should be rounded down and the sup bound should be rounded up *)
      match q_inf, q_sup with
      | (1, 1 | 4, 1 | 4, 4) -> Interval.of_float sin_inf sin_sup
      | (2, 2 | 2, 3 | 3, 3) -> Interval.of_float sin_sup sin_inf
      | (3, 2 | 1, 4) -> minus_plus_one
      | (1, 2 | 4, 3) -> Interval.of_float (min sin_inf sin_sup) 1.
      | (2, 1 | 3, 4) -> Interval.of_float (-1.) (max sin_inf sin_sup)
      | 1, 3 -> Interval.of_float sin_sup 1.
      | 2, 4 -> Interval.of_float (-1.) sin_inf
      | 3, 1 -> Interval.of_float (-1.) sin_sup
      | 4, 2 -> Interval.of_float sin_inf 1.
      | _ -> failwith ("Should not occur")

(** Evaluate the cos function over an interval *)
let evaluate_cos itv =
  evaluate_sin (add itv pi_half)

(** Evaluate the tan function over an interval *)
let evaluate_tan itv =
  let diam = diam_interval itv in
  if (Mpqf.cmp diam two_pi) >= 0 then
    Interval.top
  else
    let itv' = scale_to_two_pi_itv itv in
    let inf = scalar_to_mpqf itv'.Interval.inf
    and sup = scalar_to_mpqf itv'.Interval.sup
    and diam = diam_interval itv' in
    let q_inf = quadrant inf
    and q_sup = quadrant sup in
    if q_inf = q_sup && (Mpqf.cmp diam pi) >= 0 then
      Interval.top
    else
      let inf_flt = Mpqf.to_float inf
      and sup_flt = Mpqf.to_float sup in
      match q_inf, q_sup with
      | (1, 1 | 2, 2 | 3, 3 | 4, 4 | 2, 3 | 4, 1) -> Interval.of_float (tan inf_flt) (tan sup_flt)
      | (1 | 2 | 3 | 4), (1 | 2 | 3 | 4) -> Interval.top
      | _ -> failwith ("Should not occur")

(** Evaluate the cot function over an interval *)
let evaluate_cot itv =
  let itv' = evaluate_tan (add itv pi_half) in
  Interval.neg itv'

(** Evaluate the arcsin function over an interval *)
let evaluate_asin itv =
  let inf = scalar_to_float itv.Interval.inf
  and sup = scalar_to_float itv.Interval.sup in
  if inf < (-1.) && sup > 1. then
    failwith ("Interval out of range for arcsin")
  else
    Interval.of_float (asin inf) (asin sup)

(** Evaluate the arccos function over an interval *)
let evaluate_acos itv =
  let inf = scalar_to_float itv.Interval.inf
  and sup = scalar_to_float itv.Interval.sup in
  if inf < (-1.) && sup > 1. then
    failwith ("Interval out of range for arccos")
  else
    Interval.of_float (acos inf) (acos sup)

(** Evaluate the arctan function over an interval *)
let evaluate_atan itv =
  let inf = scalar_to_float itv.Interval.inf
  and sup = scalar_to_float itv.Interval.sup in
  Interval.of_float (atan inf) (atan sup)
