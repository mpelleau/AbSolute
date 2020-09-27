(** This wrapper lifts the filtering and the satisfies function over arithmetical
    predicates (e1 < e2) to boolean formulas of the form (p1 \/ p2) *)
open Signature
open Csp
open Consistency

(** Boolean expressions abstractions *)
module Make (Abs:AbstractCP) = struct

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
                 let union = Abs.join n1 n2 in
                 Filtered ((union,false))))
      | And(b1,b2) ->
         (match loop num b1 with
          | Unsat -> Unsat
          | Sat -> loop num b2
          | Filtered (n1,sat1) as x ->
             (match loop n1 b2 with
              | Sat -> x
              | Unsat -> Unsat
              | Filtered (n2,sat2) -> Filtered (n2,sat1 && sat2)))
      | Not _ -> assert false
    in loop num c

end
