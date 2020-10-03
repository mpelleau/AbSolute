(** This wrapper lifts the filtering and the satisfies function over
   arithmetical predicates (e1 < e2) to boolean formulas of the form
   (p1 \/ p2) *)

open Signature
open Csp
open Consistency

(** Boolean expressions abstractions *)
module Make (Abs:Numeric) : Domain = struct
  include Abs

  type internal_constr = Abs.internal_constr Csp.boolean

  let internalize ?elem = Csp_helper.map_constr (Abs.internalize ?elem)
  let externalize = Csp_helper.map_constr Abs.externalize

  let filter (num:Abs.t) c : Abs.t Consistency.t =
    let rec loop num c =
      match c with
      | Cmp a -> Abs.filter num a
      | Or (b1,b2) ->
         (match loop num b1 with
          | Sat -> Sat
          | Unsat -> loop num b2
          | Filtered (n1,_) as x ->
             (match loop num b2 with
              | Sat -> Sat
              | Unsat -> x
              | Filtered (n2,_) ->
                 let union,exact = Abs.join n1 n2 in
                 Filtered ((union,exact))))
      | And(b1,b2) -> Consistency.fold_and loop num [b1;b2]
      | Not _ -> assert false
    in loop num c

  let rec is_representable = function
    | And(a, b) -> Kleene.and_kleene (is_representable a) (is_representable b)
    | Cmp c -> Abs.is_representable c
    | _ -> Kleene.False
end
