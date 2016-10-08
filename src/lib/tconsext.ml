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
