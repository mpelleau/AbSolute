open Apron
open Apronext

(** {1 Conversion operators } *)

let itv_to_mpqf i = Interval.(Scalarext.(to_mpqf i.inf,to_mpqf i.sup))

let coeff_to_int x = Coeffext.to_int x
(** {1 Operators } *)

let split_prec_mpqf = Mpqf.of_float !Constant.precision

let sqrt2 = 0.707106781186548
let sqrt2_mpqf = Mpqf.of_float sqrt2

let scalar_mul_sqrt2 sca =
  let value = Scalarext.to_mpqf sca in
  let mult = Mpqf.mul value sqrt2_mpqf in
  Scalar.of_mpqf mult


(* Compute the euclidian distance between two scalars *)
let diam inf sup =
  let mpqf_inf = Scalarext.to_mpqf inf in
  let mpqf_sup = Scalarext.to_mpqf sup in
  Mpqf.sub mpqf_sup mpqf_inf

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

type point = float array

(* Compute the square of the euclidian distance between two points. *)
let sq_dist p1 p2 =
  let sum = ref 0. in
  Array.iter2 (fun a b -> sum := !sum +. (a -. b) ** 2.) p1 p2;
  !sum

(* compute the most distant pair of points of an array of points,
     and the corresponding distance*)
let most_distant_pair (pts:point array) : point * point * float =
  fold_on_combination_2 (fun ((_,_,dist) as old) p1 p2 ->
      let dist' = sq_dist p1 p2 in
      if dist' > dist then (p1,p2,dist')
      else old) (pts.(0),pts.(0),0.) pts
