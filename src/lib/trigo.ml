(******************************************************************)
(******* Trigonometric utilities using OCaml floatting type *******)
(* We'll use interval to do every computation to ensure soundness *)
(********* we'll also use partial approximation by series *********)
(******************************************************************)

let sum i n f =
  let rec aux acc cur =
    if cur > n then acc
    else aux (acc +. (f cur)) (cur+1)
  in aux 0. i

let fact n =
  let rec aux acc = function
    | 0 | 1 -> acc
    | x -> aux (acc * x) (x-1)
  in aux 1 n

module Make(B:Bound_sig.BOUND) = struct

  let toB (a,b) = (B.of_float_down a),(B.of_float_up b)
  (*the two closest floating boundaries of pi*)
  let pi_up = B.of_float_up 3.14159265358979356
  let pi_down = B.of_float_down 3.14159265358979312

  let pi = pi_down,pi_up

  let n_pi n =
    (B.mul_down pi_down (B.of_int_down n)),
    (B.mul_up pi_up (B.of_int_up n))

  let pi_tab = Array.make 1000 (B.zero,B.zero)

  let init () =
    Array.iteri (fun i _ -> pi_tab.(i) <- n_pi (i+1)) pi_tab

  let sin m (x,n) =
    sum 1 m (fun i ->
      (if i mod 2 = 0 then -1. else 1.)
      *. (x ** float (2*i-1))
      /. (fact (2*i - 1) |> float)
    )

  let cos m (x,n) =
    1. +.
      sum 1 m (fun i ->
        (if i mod 2 = 0 then 1. else -1.)
        *. (x ** float (2*i))
        /. (fact (2*i) |> float)
      )

  let sin_up (x,n) = sin (if x < 0. then 2*n else 2*n+1) (x,n)
  let sin_down (x,n) = sin ((if x < 0. then 2*n else 2*n+1)+1) (x,n)

  let cos_up (x,n) = cos ((if x < 0. then 2*n else 2*n+1)+1) (x,n)
  let cos_down (x,n) = cos (if x < 0. then 2*n else 2*n+1) (x,n)

  let sin_up x = sin_up (x,7) |> B.of_float_down
  let sin_down x = sin_down (x,7) |> B.of_float_up

  let cos_up x = cos_up (x,6) |> B.of_float_down
  let cos_down x = cos_down (x,6) |> B.of_float_up

  let hpi_low = 1.57079632 and hpi_high = 1.57079633
  let tpi_low = 6.28318530 and tpi_high = 6.28318531
  let thpi_low = 4.7123889 and thpi_high = 4.7123890
  let half_pi = toB (hpi_low,hpi_high)
  let two_pi = toB (tpi_low,tpi_high)
  let three_half_of_pi = toB (thpi_low,thpi_high)

  type quad = | PosXY
              | PosXNegY
              | PosYNegX
              | NegXY
              | PosX        (* around zero *)
              | PosY        (* around half of pi*)
              | NegX        (* around -pi*)
              | NegY        (* around 3/2 pi*)

  let range_reduction x = x

  let check_quad (a,b) =
    let (a,b) = (B.of_float_down a),(B.of_float_up b) in
    let contains (a,b) (c,d) = B.leq a c && B.leq d b in
    let quad x = List.find (fun (itv,quad) -> contains itv x) [
      (half_pi, PosY);
      (three_half_of_pi, NegY);
      (two_pi, PosX);
      (pi, NegX);
      (toB(0.,hpi_low),PosXY)
    ] |> snd
    in quad (a,b)

  let get_range (a,b) = function
    | _ -> failwith ""

  let sin (a,b) =
    (* 4.8 > 3pi/2 *)
    if b -. a > 4.8 then (-1.,1.)
    else (a,b) |> range_reduction |> check_quad |> get_range (a,b)
end
