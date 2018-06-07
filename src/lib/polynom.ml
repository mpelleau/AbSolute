(*************************************************************************)
(* This modules handles symbolic computations over multivariate polynoms *)
(* It is parametrized by a ring module which deals with basic arithmetic *)
(*************************************************************************)

module type Ring = sig
  type t
  val add   : t -> t -> t
  val mul   : t -> t -> t

  (* None if the division is not exact *)
  val div   : t -> t -> t option

  (* None if the value cannot be converted exactly to an integer *)
  val to_int : t -> int option
  val to_float : t -> float
  val to_rational : t -> Mpqf.t
  val floor : t -> int

  val zero  : t
  val one   : t

  (* entry points *)
  val of_int : int -> t
  val of_float : float -> t
  val of_rational : Mpqf.t -> t

  (* printintg *)
  val print : Format.formatter -> t -> unit
end

module Make(R:Ring) = struct

  type t      = cell list          (* monoms list <- sorted in lex. order *)
   and cell   = coeff * var list   (* c * v1*...*vn <- sorted in lex. order *)
   and var    = id * exp           (* id^exp *)
   and id     = string
   and exp    = R.t
   and coeff  = R.t

  (* if a monom correspont to a constant *)
  let is_monom_constant ((c,v):cell) =
    match v with
    | [] -> true
    | _ -> false

  (**********************)
  (* printing utilities *)
  (**********************)

  let clean (poly:t) : t =
    let remove_null : t = List.filter (fun (c,e) -> c<>R.zero) poly in
    List.sort (fun (_,v1) (_,v2) -> if v1 >  v2 then 1 else -1) remove_null

  let print_varlist fmt =
    List.iter (fun (p,e) ->
        if e = R.one then Format.fprintf fmt "%s" p
        else Format.fprintf fmt "%s^%a " p R.print e)

  let print_cell fmt (((coeff,varl) as c) :cell) =
    if is_monom_constant c then Format.fprintf fmt "%a" R.print coeff
    else Format.fprintf fmt "%a%a" R.print coeff print_varlist varl

  let print fmt p =
    Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.fprintf fmt "+")
      (fun fmt -> Format.fprintf fmt "%a" print_cell) fmt
      (clean p)

  (*******************************************************)
  (*           CONSTRUCTORS AND CONSTANTS                *)
  (*******************************************************)

  (* empty list of variable means constant *)
  let of_int x : t = [(R.of_int x),[]]
  let of_float x : t = [(R.of_float x),[]]
  let of_rational x : t = [(R.of_rational x), []]

  let to_int = R.floor
  let to_float = R.to_float
  let to_rational = R.to_rational

  let monomzero : cell = R.zero,[]

  let zero : t = [monomzero]

  let one : t = [(R.one,[])]

  let of_var v : t = [(R.one,[(v,R.one)])]

  (* convert a monom to a constant *)
  let to_constant (((c,v) as monom):cell) =
    if is_monom_constant monom then c
    else
      let err_msg =
        Format.sprintf
          "can't convert the monom to a constant"
      in
      raise (Invalid_argument err_msg)

  (**************)
  (* arithmetic *)
  (**************)

  (* checks if the variable of two monoms appear with the same exponent *)
  let compare_cells ((_,vars1):cell) ((_,vars2):cell) = vars1=vars2

  (* add two monoms of same variables *)
  let add_cell_cell ((c1,v1):cell) ((c2,v2):cell) : cell =
    if v1 = v2 then ((R.add c1 c2), v1)
    else failwith "add_cell_cell : v1 should be equal to v2"

  (* add one polynom with one monom *)
  let add_cell (e:t) (((c,v) as cell):cell) : t =
    let rec browse = function
      | [] -> [cell]
      | (c',v')::tl ->
         if v = v' then
           let newc = R.add c' c in
           if newc = R.zero then tl
           else (newc,v)::tl
         else (c',v')::(browse tl)
    in browse e

  (* add two polynoms *)
  let add (e1:t) (e2:t) : t =
    List.fold_left add_cell e1 e2

  (* multiplication of two monoms *)
  let mul_cell_cell ((c1,vars1):cell) ((c2,vars2):cell) : cell =
    let rec mul_list l1 l2 =
      match l1,l2 with
      | [],[]       -> []
      | [],x | x,[] -> x
      | ((p1,e1) as v1)::t1, ((p2,e2) as v2)::t2 ->
         (* we keep the variables in a sorted order *)
         if p1 = p2 then (p1,(R.add e1 e2))::(mul_list t1 t2)
         else if p1 < p2 then v1::(mul_list t1 l2)
         else v2::(mul_list l1 t2)
    in
    let coeff = R.mul c1 c2 in
    if coeff = R.zero then monomzero
    else coeff, (mul_list vars1 vars2)

  (* multiplication of one polynom with one monom *)
  let mul_ex_cell (e:t) (c:cell) : t =
    List.map (fun e -> mul_cell_cell c e) e

  (* multiplication  of two polynoms *)
  let mul (e1:t) (e2:t) =
    List.fold_left(fun acc c -> add acc (mul_ex_cell e1 c)) zero e2

  (* polynom negation *)
  let neg (e1:t) : t =
    mul e1 (of_int (-1))

  (* substraction of two polynoms *)
  let sub (e1:t) (e2:t) : t = add e1 (neg e2)

  (* check if a is divisible by b *)
  let is_divisible a b = false

  (* division of two polynoms *)
  (* return None if the division is not exact or if e2 = 0 *)
  let div (e1:t) (e2:t) : t option =
    try
      match e2 with
      | [c] -> if is_monom_constant c then
                 let d = to_constant c in
                 let p' =
                   List.map (fun (c,vl) ->
                       match R.div c d with
                       | None -> raise Exit
                       | Some c -> (c,vl)
                     ) e1
                 in Some p'
               else None
      | _ -> None
    with Exit -> None

  (* exponentation of two polynoms *)
  (* return None if the exponent is not constant *)
  let pow (e1:t) (e2:t) : t option =
    match e2 with
    | [c] -> if is_monom_constant c then
               match to_constant c |> R.to_int with
               | Some d ->
                  let rec aux acc d =
                    if d = 0 then acc
                    else aux (mul acc e1) (d-1)
                  in
                  let res = aux (of_int 1) d in
                  Some res
               | None -> None
             else None
    | _ -> None
end

module IntRing = struct

  type t = int
  let add = ( + )
  let mul = ( * )
  let zero = 0
  let one = 1

  let div x y = if y <> 0 && x mod y = 0 then Some (x/y) else None

  let to_int x = Some x
  let to_float = float_of_int
  let to_rational = Mpqf.of_int
  let floor x = x

  let of_int x = x
  let of_float = int_of_float
  let of_rational x = of_float (Mpqf.to_float x)

  let print fmt x = Format.fprintf fmt "%i" x

end


module FloatRing = struct

  type t = float
  let add = ( +. )
  let mul = ( *. )
  let zero = 0.
  let one = 1.

  let div x y = if y <> 0. && (x/.y) *. y = x then Some (x/.y) else None

  let to_int x =
    let xi = int_of_float x in
    if float xi = x then Some xi
    else None
  let to_float x = x
  let to_rational = Mpqf.of_float
  let floor = int_of_float

  let of_int = float_of_int
  let of_float x = x
  let of_rational = Mpqf.to_float

  let print fmt x = Format.fprintf fmt "%f" x

end

module RationalRing = struct

  type t = Mpqf.t
  let add = Mpqf.add
  let mul = Mpqf.mul
  let zero = Mpqf.of_int 0
  let one = Mpqf.of_int 1

  let div x y = if not (Mpqf.equal zero y) then Some (Mpqf.div x y) else None

  let to_int x =
    let xi = int_of_float (Mpqf.to_float x) in
    if Mpqf.equal x (Mpqf.of_int xi) then Some xi
    else None
  let to_float = Mpqf.to_float
  let to_rational x = x
  let floor x = to_float x |> int_of_float

  let of_int = Mpqf.of_int
  let of_float = Mpqf.of_float
  let of_rational x = x

  let print fmt x = Format.fprintf fmt "%s" (Mpqf.to_string x)
               
          
end
               

module Int = Make(IntRing)
module Float = Make(FloatRing)
module Rational = Make(RationalRing)
