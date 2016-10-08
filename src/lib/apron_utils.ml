open Apron
(******************************************************************)
(***************** Different conversion operators *****************)
(******************************************************************)

let scalar_to_mpqf = function
  | Scalar.Mpqf x -> x
  | Scalar.Float x -> Mpqf.of_float x
  | Scalar.Mpfrf x -> Mpfrf.to_mpqf x

let scalar_to_float s =
  let res = match s with
  | Scalar.Mpqf x -> Mpqf.to_float x
  | Scalar.Float x -> x
  | Scalar.Mpfrf x -> Mpfrf.to_float ~round:Mpfr.Near x
  in res

let scalar_to_int x = scalar_to_float x |> int_of_float

let coeff_to_float = function
  | Coeff.Scalar x -> scalar_to_float x
  | Coeff.Interval i -> scalar_to_float i.Interval.inf

let coeff_to_int x = coeff_to_float x |> int_of_float

(**********************)
(* Printing utilities *)
(**********************)
let print_sol box =
  let open Interval in
  let open Format in
  let itv = box.Abstract1.interval_array in
  printf "[| ";
  Array.iter (fun e -> printf "[%f; %f];" (scalar_to_float e.inf) (scalar_to_float e.sup)) itv;
  printf "|] "


(**********************)
(*   APRON Utilities  *)
(**********************)

let empty_env = Environment.make [||] [||]

let array_fold get len f a arr =
  let size = len arr in
  let rec aux accu idx =
    if idx >= size then accu
    else aux (f accu (get arr idx)) (idx+1)
  in aux a 0

let tcons_aiter f tcons =
  for i = 0 to Tcons1.array_length tcons -1 do
    Tcons1.array_get tcons i |> f
  done

let lincons_aiter f lcons =
  for i = 0 to Lincons1.array_length lcons - 1 do
    Lincons1.array_get lcons i |> f
  done

(* Tcons earray utilities *)
let tcons_for_all pred tcons =
  let open Tcons1 in
  try
    for i = 0 to (array_length tcons) -1 do
      if array_get tcons i |> pred |> not then raise Exit
    done;
    true
  with Exit -> false

let tcons_list_to_earray tcl =
  let open Tcons1 in
  let size = List.length tcl
  and env = (List.hd tcl).env in
  let ear = array_make env size in
  List.iteri (fun i b -> array_set ear i b) tcl;
  ear

let earray_to_list get length tcl =
  let l = ref [] in
  for i = 0 to (length tcl) - 1 do
    l:=(get tcl i)::!l
  done;
  !l

let lincons_earray_to_list =
  earray_to_list Lincons1.array_get Lincons1.array_length

(* let tcons_earray_to_list = *)
(*   earray_to_list Tcons1.array_get Tcons1.array_length *)

let neg_typ = let open Lincons1 in function
  | EQ -> DISEQ
  | SUP -> SUPEQ
  | SUPEQ -> SUP
  | DISEQ -> EQ
  | _ -> assert false

(* constructs a new contraint in opposite direction *)
  let neg_lincons (d:Lincons1.t) : Lincons1.t =
    let d = Lincons1.copy d in
    Lincons1.set_cst d (Coeff.neg (Lincons1.get_cst d));
    Lincons1.iter (fun c v -> Lincons1.set_coeff d v (Coeff.neg c)) d;
    Lincons1.set_typ d (Lincons1.get_typ d |> neg_typ);
    d

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
  if Array.length tab1 != Array.length tab2 then
    failwith ("The two arrays must have the same length.")
  else
    let sum = ref 0. in
    for i=0 to ((Array.length tab1)-1) do
      sum := !sum +. ((tab1.(i) -. tab2.(i)) ** 2.)
    done;
    sqrt !sum

(* Compute the maximal distance between a point and an array of points.
 * A point correspond to an array of floats.
 *)
let maxdist point tabpoints =
  let length = Array.length tabpoints in
  let rec maxd i point_max i_max dist_max =
    if i >= length then (point_max, i_max, dist_max)
    else
      let dist_i = dist tabpoints.(i) point in
      if dist_i > dist_max then
        maxd (i+1) tabpoints.(i) i dist_i
      else
        maxd (i+1) point_max i_max dist_max
  in
  let (point_max, i_max, dist_max) = maxd 1 tabpoints.(0) 0 (dist tabpoints.(0) point) in
  (point_max, i_max, dist_max)

(* Compute the maximal distance between two points in an array of points.
   A point correspond to an array of floats *)
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


(* Converts a linear expression into its negation
 * ex: converts 3x-y into -3x+y
 *)
let linexpr_neg linexpr env =
  let linexpr' = Linexpr1.make env in
  let list = ref [] in
  let cst = Linexpr1.get_cst linexpr in
  Linexpr1.iter (fun c -> fun v -> list := List.append !list [(Coeff.neg c, v)]) linexpr;
  Linexpr1.set_list linexpr' !list (Some (Coeff.neg cst));
  linexpr'


(* Converts a Generator0 into an array of floats. *)
let to_float_array gen size =
  let tab = Array.make size 0. in
  let gen_lin = gen.Generator0.linexpr0 in
  for i=0 to (size-1) do
    let coeff = Linexpr0.get_coeff gen_lin i in
    tab.(i) <- coeff_to_float coeff
  done;
  tab

(* Converts a Generator1 into an array of array of floats. *)
let gen_to_array gens size =
  let gen_tab = gens.Generator1.generator0_array in
  let tab = Array.make (Array.length gen_tab) (Array.make size 0.) in
  for i=0 to ((Array.length gen_tab)-1) do
    tab.(i) <- to_float_array gen_tab.(i) size
  done;
  tab
