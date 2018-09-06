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
  let d = copy d in set_typ d (get_typ d |> neg_typ);
  d

(* split a = into a > b or a < b*)
let splitdiseq c =
  let c1 = copy c in
  set_typ c1 SUP;
  let texpr = get_texpr1 c in
  let texpr' = Apron.Texpr1.unop Apron.Texpr0.Neg texpr  Apron.Texpr0.Real  Apron.Texpr0.Near in
  let c2 = Apron.Tcons1.make texpr' SUP in
  c1,c2
