include Apron.Tcons1
include Array_maker.TconsExt

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
  set_typ d (get_typ d |> neg_typ);
  d

(* split a = into a > b or a < b*)
let splitdiseq c =
  let open Apron in
  let c1 = copy c in
  set_typ c1 SUP;
  let texpr = get_texpr1 c in
  let texpr' = Texpr1.unop Texpr0.Neg texpr Texpr0.Real  Texpr0.Near in
  let c2 = Tcons1.make texpr' SUP in
  c1,c2
