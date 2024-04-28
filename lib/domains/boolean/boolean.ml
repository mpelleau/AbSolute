(** This wrapper lifts the filtering and the satisfies function over
    arithmetical predicates (e1 < e2) to boolean formulas of the form (p1 \/ p2) *)

open Signature
open Consistency
open Constraint

(** Boolean expressions abstractions *)
module Make (Abs : Numeric) : Domain = struct
  include Abs

  type internal_constr = Abs.internal_constr Constraint.boolean

  let internalize ?elem c =
    let c' = Constraint.remove_not c in
    Csp_helper.map_constr (Abs.internalize ?elem) c'

  let externalize = Csp_helper.map_constr Abs.externalize

  let rec sat i = function
    | Cmp a -> Abs.sat i a
    | And (b1, b2) -> sat i b1 && sat i b2
    | Or (b1, b2) -> sat i b1 || sat i b2
    | Not b -> not (sat i b)

  (* filter on boolean formulae. It also computes a simplified boolean formulae
     e.g: - false || c <=> c - true && c <=> c *)
  let rec filter (num : Abs.t) c =
    match c with
    | Cmp a -> Consistency.map (fun x -> (x, c)) (Abs.filter num a)
    | Or (b1, b2) -> (
      match filter num b1 with
      | Sat -> Consistency.Sat
      | Unsat -> filter num b2
      | Filtered ((n1, b1'), sat1) as x -> (
        match filter num b2 with
        | Sat -> Sat
        | Unsat -> x
        | Filtered ((n2, b2'), sat2) ->
            let union, exact = Abs.join n1 n2 in
            Filtered ((union, Or (b1', b2')), sat1 && sat2 && exact)
        | _ -> failwith "pruned" )
      | _ -> failwith "pruned" )
    | And (b1, b2) -> (
      match filter num b1 with
      | Unsat -> Unsat
      | Sat -> filter num b2
      | Filtered ((n1, b1'), sat1) as x -> (
        match filter n1 b2 with
        | Sat -> x
        | Unsat -> Unsat
        | Filtered ((num', b2'), sat2) ->
            Filtered ((num', And (b1', b2')), sat1 && sat2)
        | _ -> failwith "pruned" )
      | _ -> failwith "pruned" )
    | Not _ -> assert false

  (* filter on boolean formulae. It also computes a simplified boolean formulae
     e.g: - false || c <=> c - true && c <=> c *)
  let rec filter_diff (num : Abs.t) c =
    match c with
    | Cmp a ->
        Consistency.map (fun (x, vars) -> (x, c, vars)) (Abs.filter_diff num a)
    | Or (b1, b2) -> (
      match filter_diff num b1 with
      | Sat -> Consistency.Sat
      | Unsat -> filter_diff num b2
      | Filtered ((n1, b1', v1), sat1) as x -> (
        match filter_diff num b2 with
        | Sat -> Sat
        | Unsat -> x
        | Filtered ((n2, b2', v2), sat2) ->
            let union, exact = Abs.join n1 n2 in
            Filtered
              ( (union, Or (b1', b2'), Tools.VarSet.union v1 v2)
              , sat1 && sat2 && exact )
        | _ -> failwith "pruned" )
      | _ -> failwith "pruned" )
    | And (b1, b2) -> (
      match filter_diff num b1 with
      | Unsat -> Unsat
      | Sat -> filter_diff num b2
      | Filtered ((n1, b1', v1), sat1) as x -> (
        match filter_diff n1 b2 with
        | Sat -> x
        | Unsat -> Unsat
        | Filtered ((num', b2', v2), sat2) ->
            Filtered
              ((num', And (b1', b2'), Tools.VarSet.union v1 v2), sat1 && sat2)
        | _ -> failwith "pruned" )
      | _ -> failwith "pruned" )
    | Not _ -> assert false

  let rec is_representable = function
    | And (a, b) -> Kleene.and_ (is_representable a) (is_representable b)
    | Cmp c -> Abs.is_representable c
    | _ -> Kleene.False
end
