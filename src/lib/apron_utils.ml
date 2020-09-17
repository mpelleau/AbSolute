open Apron
(******************************************************************)
(***************** Different conversion operators *****************)
(******************************************************************)

let scalar_to_mpqf = function
  | Scalar.Mpqf x -> x
  | Scalar.Float x -> Mpqf.of_float x
  | Scalar.Mpfrf x -> Mpfrf.to_mpqf x

let scalar_to_float = function
  | Scalar.Mpqf x -> Mpqf.to_float x
  | Scalar.Float x -> x
  | Scalar.Mpfrf x -> Mpfrf.to_float ~round:Mpfr.Near x

let scalar_to_int x = scalar_to_float x |> int_of_float

let itv_to_float i = (scalar_to_float i.Interval.inf, scalar_to_float i.Interval.sup)

let itv_to_mpqf i = (scalar_to_mpqf i.Interval.inf, scalar_to_mpqf i.Interval.sup)

let coeff_to_float = function
  | Coeff.Scalar x -> scalar_to_float x
  | Coeff.Interval i -> scalar_to_float i.Interval.inf

let coeff_to_mpqf = function
  | Coeff.Scalar x -> scalar_to_mpqf x
  | Coeff.Interval i -> scalar_to_mpqf i.Interval.inf

let coeff_to_int x = coeff_to_float x |> int_of_float

(**********************)
(*   APRON Utilities  *)
(**********************)

let empty_env = Environment.make [||] [||]

(******************************************************************)
(*********************** Various operators ************************)
(******************************************************************)

let split_prec = 0.00001
let split_prec_mpqf = Mpqf.of_float split_prec

let sqrt2 = 0.707106781186548
let sqrt2_mpqf = Mpqf.of_float sqrt2

(* Compute the sum of two scalars *)
let scalar_add sca sca' =
  let value = scalar_to_mpqf sca in
  let value' = scalar_to_mpqf sca' in
  let sum = Mpqf.add value value' in
  Scalar.of_mpqf sum

(* Compute the sum of two scalars *)
let scalar_mul_sqrt2 sca =
  let value = scalar_to_mpqf sca in
  let mult = Mpqf.mul value sqrt2_mpqf in
  Scalar.of_mpqf mult

(* Compute the sum of a scalar and a Mpqf *)
let scalar_plus_mpqf sca mpqf =
  let value = scalar_to_mpqf sca in
  let sum = Mpqf.add value mpqf in
  Scalar.of_mpqf sum

(* Compute the medium value of two scalars *)
let mid inf sup =
  let mpqf_inf = scalar_to_mpqf inf
  and mpqf_sup = scalar_to_mpqf sup in
  let mid =
    let div_inf = Mpqf.div mpqf_inf (Mpqf.of_int 2)
    and div_sup = Mpqf.div mpqf_sup (Mpqf.of_int 2)
    in Scalar.of_mpqf (Mpqf.add div_inf div_sup)
  in mid
  (* Scalar.of_mpqf (Mpqf.div (Mpqf.add mpqf_inf mpqf_sup) (Mpqf.of_int 2)) *)

(* Compute the middle value of an interval *)
let mid_interval itv =
  mid itv.Interval.inf itv.Interval.sup


(* Compute the euclidian distance between two scalars *)
let diam inf sup =
  let mpqf_inf = scalar_to_mpqf inf in
  let mpqf_sup = scalar_to_mpqf sup in
  Mpqf.sub mpqf_sup mpqf_inf

(* Compute the diameter of an interval *)
let diam_interval itv =
  diam itv.Interval.inf itv.Interval.sup

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
