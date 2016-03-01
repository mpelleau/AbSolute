open Apron

(**********************)
(* Printing utilities *)
(**********************)
let print_array = Abstract0.print_array

let lincons1_array_print fmt x = Lincons1.array_print fmt x

let generator1_array_print fmt x = Generator1.array_print fmt x

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

let coeff_to_float = function
  | Coeff.Scalar x -> scalar_to_float x
  | Coeff.Interval i -> scalar_to_float i.Interval.inf
