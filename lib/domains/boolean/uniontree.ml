open Signature
open Consistency

(* This module implements the tree abstract domain described in "Abstract
   Domains for Constraint Programming with Differential Equations". It provides
   an efficient pre-calculus for meets and filter on large disjunctions, based
   on dichotomy and convex hulls. *)

module Make (D : Numeric) : Domain = struct
  type t = Leaf of D.t | Union of {envelopp: D.t; sons: t * t}

  type internal_constr = D.internal_constr Constraint.boolean

  let internalize ?elem c =
    match elem with
    | Some (Leaf n) ->
        let c' = Constraint.remove_not c in
        Csp_helper.map_constr (D.internalize ~elem:n) c'
    | _ -> failwith "of_comparison should be called on a leaf"

  let externalize = Csp_helper.map_constr D.externalize

  let rec is_representable = function
    | Constraint.And (a, b) | Or (a, b) ->
        Kleene.and_ (is_representable a) (is_representable b)
    | Constraint.Not e -> is_representable e
    | Constraint.Cmp c -> D.is_representable c

  let forward_eval = function
    | Leaf n -> D.forward_eval n
    | Union {envelopp; _} -> D.forward_eval envelopp

  (* helper that folds over tree *)
  let fold f acc =
    let rec fold acc = function
      | Leaf n -> f acc n
      | Union {sons= l, r; _} -> fold (fold acc l) r
    in
    fold acc

  let rec print fmt = function
    | Leaf d -> D.print fmt d
    | Union {sons= l, r; _} -> Format.fprintf fmt "(%a || %a)" print l print r

  let map f t =
    let rec loop = function
      | Leaf l -> Leaf (f l)
      | Union {envelopp; sons= l, r} ->
          Union {envelopp= f envelopp; sons= (loop l, loop r)}
    in
    loop t

  let add_var t v = map (fun n -> D.add_var n v) t

  let rm_var t v = map (fun n -> D.rm_var n v) t

  (* all elements defined on the same set of variables *)
  let vars = function Leaf e | Union {envelopp= e; _} -> D.vars e

  let empty = Leaf D.empty

  let bounding = function Leaf n -> n | Union {envelopp; _} -> envelopp

  (* constructors *)
  let leaf x = Leaf x

  let union e a b = Union {envelopp= e; sons= (a, b)}

  (* TODO: improve when exact join is met *)
  let join a b =
    let hull, _exact = D.join (bounding a) (bounding b) in
    (Union {envelopp= hull; sons= (a, b)}, true)

  let rec meet q1 q2 =
    match (q1, q2) with
    | Leaf e1, Leaf e2 -> Option.map leaf (D.meet e1 e2)
    | Union {envelopp; sons= l, r}, b | b, Union {envelopp; sons= l, r} -> (
      match D.meet envelopp (bounding b) with
      | None -> None
      | Some _ ->
          Tools.join_bot2 (fun a b -> fst (join a b)) (meet b l) (meet b r) )

  let meet_env hull = function
    | Leaf e -> Option.map leaf (D.meet e hull)
    | Union ({envelopp; _} as u) ->
        D.meet envelopp hull
        |> Option.map (fun b' -> Union {u with envelopp= b'})

  (* filter for numeric predicates *)
  let filter_cmp (t : t) cmp : t Consistency.t =
    match t with
    | Leaf e -> Consistency.map leaf (D.filter e cmp)
    | Union {envelopp; sons= l, r} ->
        D.filter envelopp cmp
        |> Consistency.bind (fun e' x ->
               match
                 Tools.join_bot2 (union e') (meet_env e' l) (meet_env e' r)
               with
               | None -> Unsat
               | Some e -> Filtered (e, x))

  (* filter for boolean expressions *)
  let filter (n : t) c : (t * internal_constr) Consistency.t =
    let open Constraint in
    let rec loop e = function
      | Cmp a -> Consistency.map (fun x -> (x, c)) (filter_cmp e a)
      | Or (b1, b2) -> (
        match loop e b1 with
        | Sat -> Sat
        | Unsat -> loop e b2
        | Filtered ((n1, b1'), sat1) as x -> (
          match loop e b2 with
          | Sat -> Sat
          | Unsat -> x
          | Filtered ((n2, b2'), sat2) ->
              let union, exact = join n1 n2 in
              Filtered ((union, Or (b1', b2')), sat1 && sat2 && exact) ) )
      | And (b1, b2) -> (
        match loop e b1 with
        | Unsat -> Unsat
        | Sat -> loop e b2
        | Filtered ((n1, b1'), sat1) as x -> (
          match loop n1 b2 with
          | Sat -> x
          | Unsat -> Unsat
          | Filtered ((num', b2'), sat2) ->
              Filtered ((num', And (b1', b2')), sat1 && sat2) ) )
      | Not _ -> assert false
    in
    loop n c

  let split prec = function
    | Leaf x -> List.rev_map leaf (D.split prec x)
    | Union {sons= l, r; _} -> [l; r]

  let is_abstraction t i =
    try
      fold (fun () n -> if D.is_abstraction n i then raise Exit) () t ;
      false
    with Exit -> true

  (* fast overapprox *)
  let volume = function Leaf x | Union {envelopp= x; _} -> D.volume x

  let rec spawn = function
    | Leaf l -> D.spawn l
    | Union {sons= l, r; _} -> spawn (if Random.bool () then l else r)

  let diff = None

  let rec to_constraint = function
    | Leaf d -> D.to_constraint d
    | Union {sons= l, r; _} -> Constraint.Or (to_constraint l, to_constraint r)

  let render t =
    match fold (fun acc e -> e :: acc) [] t with
    | h :: tl ->
        List.fold_left
          (fun acc e -> Picasso.Drawable.union acc (D.render e))
          (D.render h) tl
    | _ -> failwith "cannot render bottom element"
end
