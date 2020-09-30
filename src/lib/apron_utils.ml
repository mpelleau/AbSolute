open Apron
open Apronext
open Tools

(** {1 Conversion operators } *)

let itv_to_mpqf i = Interval.(Scalarext.(to_mpqf i.inf,to_mpqf i.sup))

let scalar_mul_sqrt2 =
  let sqrt2_mpqf = Mpqf.of_float 0.707106781186548 in
  fun sca ->
  let value = Scalarext.to_mpqf sca in
  let mult = Mpqf.mul value sqrt2_mpqf in
  Scalar.of_mpqf mult

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
