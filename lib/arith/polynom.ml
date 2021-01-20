(** This modules handles symbolic computations over multivariate polynoms It is
    parametrized by a ring module which deals with basic arithmetic **)

module Make (R : Ring.T) = struct
  type t = cell list

  (* monoms list <- sorted in lex. order *)
  and cell = coeff * var list

  (* c * v1*...*vn <- sorted in lex. order *)
  and var = id * exp

  (* id^exp *)
  and id = string

  and exp = int

  and coeff = R.t

  (* if a monom correspont to a constant *)
  let is_monom_constant ((_, v) : cell) = match v with [] -> true | _ -> false

  (* if a polynom is a monom *)
  let is_monom = function [_] -> true | _ -> false

  let to_monom_opt = function [h] -> Some h | _ -> None

  (* convert a monom to a constant *)
  let monom_to_constant ((c, _) as monom : cell) =
    if is_monom_constant monom then c
    else
      let err_msg = Format.sprintf "can't convert the monom to a constant" in
      raise (Invalid_argument err_msg)

  (* convert a monom to a constant option *)
  let monom_to_constant_opt ((c, _) as monom : cell) : coeff option =
    if is_monom_constant monom then Some c else None

  (* convert a monom to a constant option *)
  let to_constant_opt (p : t) : coeff option =
    p |> to_monom_opt |> Option.map monom_to_constant_opt |> Option.join

  let is_linear (p : t) : bool =
    List.for_all (function _, [(_, 1)] -> true | _ -> false) p

  (**********************)
  (* printing utilities *)
  (**********************)

  let clean (poly : t) : t =
    let remove_null : t = List.filter (fun (c, _) -> c <> R.zero) poly in
    List.sort (fun (_, v1) (_, v2) -> if v1 > v2 then 1 else -1) remove_null

  let print_varlist fmt =
    List.iter (fun (p, e) ->
        if e = 1 then Format.fprintf fmt "%s" p
        else Format.fprintf fmt "%s^%i " p e)

  let print_cell fmt ((coeff, varl) as c : cell) =
    if is_monom_constant c then Format.fprintf fmt "%a" R.print coeff
    else Format.fprintf fmt "%a%a" R.print coeff print_varlist varl

  let print fmt p =
    Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.fprintf fmt "+")
      (fun fmt -> Format.fprintf fmt "%a" print_cell)
      fmt (clean p)

  let to_string =
    let vars_to_string : var list -> string =
     fun vars ->
      List.map
        (fun (v, e) -> if e = 1 then v else Printf.sprintf "%s^%i" v e)
        vars
      |> String.concat ""
    in
    fun p ->
      List.map
        (fun (coeff, vars) ->
          Printf.sprintf "%s.%s" (R.to_string coeff) (vars_to_string vars))
        p
      |> String.concat " + "

  (*******************************************************)
  (*           CONSTRUCTORS AND CONSTANTS                *)
  (*******************************************************)

  (* empty list of variable means constant *)
  let of_int x : t = [(R.of_int x, [])]

  let of_float x : t = [(R.of_float x, [])]

  let of_rational x : t = [(R.of_rational x, [])]

  let to_int = R.floor

  let to_float = R.to_float

  let to_rational = R.to_rational

  let monomzero : cell = (R.zero, [])

  let zero : t = [monomzero]

  let one : t = [(R.one, [])]

  let of_var v : t = [(R.one, [(v, 1)])]

  let of_constant c : t = [(c, [])]

  (**************)
  (* arithmetic *)
  (**************)

  (* add one polynom with one monom *)
  let add_cell (e : t) ((c, v) as cell : cell) : t =
    let rec browse = function
      | [] -> [cell]
      | (c', v') :: tl ->
          if v = v' then
            let newc = R.add c' c in
            if newc = R.zero then tl else (newc, v) :: tl
          else (c', v') :: browse tl
    in
    browse e

  (* add two polynoms *)
  let add (e1 : t) (e2 : t) : t = List.fold_left add_cell e1 e2

  (* multiplication of two monoms *)
  let mul_cell_cell ((c1, vars1) : cell) ((c2, vars2) : cell) : cell =
    let rec mul_list l1 l2 =
      match (l1, l2) with
      | [], [] -> []
      | [], x | x, [] -> x
      | ((p1, e1) as v1) :: t1, ((p2, e2) as v2) :: t2 ->
          (* we keep the variables in a sorted order *)
          if p1 = p2 then (p1, e1 + e2) :: mul_list t1 t2
          else if p1 < p2 then v1 :: mul_list t1 l2
          else v2 :: mul_list l1 t2
    in
    let coeff = R.mul c1 c2 in
    if coeff = R.zero then monomzero else (coeff, mul_list vars1 vars2)

  (* multiplication of one polynom with one monom *)
  let mul_ex_cell (e : t) (c : cell) : t =
    List.map (fun e -> mul_cell_cell c e) e

  (* multiplication of two polynoms *)
  let mul (e1 : t) (e2 : t) =
    List.fold_left (fun acc c -> add acc (mul_ex_cell e1 c)) zero e2

  (* polynom negation *)
  let neg (e1 : t) : t = mul e1 (of_int (-1))

  (* substraction of two polynoms *)
  let sub (e1 : t) (e2 : t) : t = add e1 (neg e2)

  (* division of two polynoms *)
  (* return None if the division is not exact or if e2 = 0 *)
  let div (e1 : t) (d : coeff) : t =
    List.map
      (fun (c, vl) ->
        match R.div c d with None -> raise Exit | Some c -> (c, vl))
      e1

  (* exponentation of a polynom *)
  let pow (p : t) (e : int) : t =
    let rec aux acc d =
      if d = 1 then acc
      else if d mod 2 = 0 then aux (mul acc acc) (d / 2)
      else aux (mul acc p) (d - 1)
    in
    aux p e
end

module Int = Make (I)
module Float = Make (F)
module Rational = Make (Q)
