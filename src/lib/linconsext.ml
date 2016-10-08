(****************************************************************************)
(* This file is an extension for the Lincons1 module from the apron Library *)
(****************************************************************************)

(* It only adds function, nothing is removed *)
include Apron.Lincons1
include Array_maker.LinconsExt

(**********************)
(* Negation utilities *)
(**********************)
let neg_typ = function
  | EQ -> DISEQ
  | SUP -> SUPEQ
  | SUPEQ -> SUP
  | DISEQ -> EQ
  | _ -> assert false

let neg d =
  let d = copy d in
  set_cst d (Apron.Coeff.neg (get_cst d));
  iter (fun c v -> set_coeff d v (Apron.Coeff.neg c)) d;
  set_typ d (get_typ d |> neg_typ);
  d

(* split a = into a >= b and a <= b*)
let spliteq c =
  let c1 = copy c in
  set_typ c1 SUPEQ;
  let c2 = copy c1 in
  set_cst c2 (Apron.Coeff.neg (get_cst c2));
  iter (fun c v -> set_coeff c2 v (Apron.Coeff.neg c)) c2;
  c1,c2

(* split a = into a > b or a < b*)
let splitdiseq c =
  let c1 = copy c in
  set_typ c1 SUP;
  let c2 = copy c1 in
  set_cst c2 (Apron.Coeff.neg (get_cst c2));
  iter (fun c v -> set_coeff c2 v (Apron.Coeff.neg c)) c2;
  c1,c2
