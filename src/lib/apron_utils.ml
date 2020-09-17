open Apron
open Apronext

(** {1 Conversion operators } *)

let scalar_to_int x = Scalarext.to_float x |> int_of_float

let itv_to_float i = (Scalarext.to_float i.Interval.inf, Scalarext.to_float i.Interval.sup)

let itv_to_mpqf i = (Scalarext.to_mpqf i.Interval.inf, Scalarext.to_mpqf i.Interval.sup)

let coeff_to_float = function
  | Coeff.Scalar x -> Scalarext.to_float x
  | Coeff.Interval i -> Scalarext.to_float i.Interval.inf

let coeff_to_mpqf = function
  | Coeff.Scalar x -> Scalarext.to_mpqf x
  | Coeff.Interval i -> Scalarext.to_mpqf i.Interval.inf

let coeff_to_int x = coeff_to_float x |> int_of_float

(** {1 Operators } *)

let split_prec_mpqf = Mpqf.of_float !Constant.precision

let sqrt2 = 0.707106781186548
let sqrt2_mpqf = Mpqf.of_float sqrt2

let scalar_mul_sqrt2 sca =
  let value = Scalarext.to_mpqf sca in
  let mult = Mpqf.mul value sqrt2_mpqf in
  Scalar.of_mpqf mult

(* Compute the sum of a scalar and a Mpqf *)
let scalar_plus_mpqf sca mpqf =
  let value = Scalarext.to_mpqf sca in
  let sum = Mpqf.add value mpqf in
  Scalar.of_mpqf sum

(* Compute the euclidian distance between two scalars *)
let diam inf sup =
  let mpqf_inf = Scalarext.to_mpqf inf in
  let mpqf_sup = Scalarext.to_mpqf sup in
  Mpqf.sub mpqf_sup mpqf_inf

(* Compute the euclidian distance between two arrays of floats. *)
let dist tab1 tab2 =
  let sum = ref 0. in
  Array.iter2 (fun a b -> sum := !sum +. (a -. b) ** 2.) tab1 tab2;
  sqrt !sum

(* Compute the max distance between a point and a array of points.
 * A point correspond to an array of floats. *)
let maxdist point tabpoints =
  let best_dist = ref (-1.)
  and best_point = ref [||]
  and best_ind = ref (-1) in
  Array.iteri (fun i p ->
    let cur_dist = dist point p in
    if cur_dist > !best_dist then begin
      best_ind := i;
      best_dist := cur_dist;
      best_point := p;
    end
  ) tabpoints;
  (!best_point,!best_ind,!best_dist)

let maxdisttab tabpoints =
  let length = Array.length tabpoints in
  let rec maxd i p1 i1 p2 i2 dist_max =
    if i >= length then (p1, i1, p2, i2, dist_max)
    else
      try
	let tabpoints' = Array.sub tabpoints (i+1) (length-i-1) in
	let (pj, j, dist) = maxdist tabpoints.(i) tabpoints' in
	if dist > dist_max then maxd (i+1) tabpoints.(i) i pj j dist
	else maxd (i+1) p1 i1 p2 i2 dist_max
      with Invalid_argument _ -> (p1, i1, p2, i2, dist_max)
  in
  let (p0, i0, dist) = maxdist tabpoints.(0) (Array.sub tabpoints 1 (length-1)) in
  let (p1, i1, p2, i2, dist_max) = maxd 1 tabpoints.(0) 0 p0 i0 dist in
  (p1, i1, p2, i2, dist_max)

(* folder on all possible combinations of size 2 of an array *)
let fold_on_combination_2 ?duplicate:(d=false) f acc arr =
  let l = Array.length arr in
  let rec aux res i1 i2 =
    if i1 >= l then res
    else if i2 = l then let n = i1+1 in aux res n n
    else if i2 = i1 && (not d) then aux res i1 (i2+1)
    else aux (f res arr.(i1) arr.(i2)) i1 (i2+1)
  in
  aux acc 0 0
