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

let coeff_to_float = function
  | Coeff.Scalar x -> scalar_to_float x
  | Coeff.Interval i -> scalar_to_float i.Interval.inf

(**********************)
(* Printing utilities *)
(**********************)
let print_array = Abstract0.print_array

let lincons1_array_print fmt x = Lincons1.array_print fmt x

let generator1_array_print fmt x = Generator1.array_print fmt x

let print_sol box =
  let open Interval in
  let open Format in
  let itv = box.Abstract1.interval_array in
  printf "[| ";
  Array.iter (fun e -> printf "[%f; %f];" (scalar_to_float e.inf) (scalar_to_float e.sup)) itv;
  printf "|] "

(* Tcons earray utilities *)
let tcons_for_all pred tcons =
  let open Tcons1 in
  try
    for i = 0 to (array_length tcons) -1  do
      if array_get tcons i |> pred |> not then raise Exit 
    done;
    true
  with Exit -> false
