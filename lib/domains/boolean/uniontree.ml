open Signature
open Consistency

(* This module implements the tree abstract domain described in "Abstract
   Domains for Constraint Programming with Differential Equations". It provides
   an efficient pre-calculus for meets and filter on large disjunctions, based
   on dichotomy and convex hulls. *)

module Make (D : Numeric) : Domain = struct
  type t = Leaf of D.t | Union of {envelopp: D.t; sons: t list}

  type internal_constr = D.internal_constr Constraint.boolean

  let internalize ?elem c =
    match elem with
    | Some (Leaf n) ->
        let c' = Constraint.remove_not c in
        Csp_helper.map_constr (D.internalize ~elem:n) c'
    | _ -> failwith "of_comparison should be called on a leaf"

  let externalize = Csp_helper.map_constr D.externalize

  let rec sat i =
    let open Constraint in
    function
    | Cmp a -> D.sat i a
    | And (b1, b2) -> sat i b1 && sat i b2
    | Or (b1, b2) -> sat i b1 || sat i b2
    | Not b -> not (sat i b)

  let rec is_representable = function
    | Constraint.And (a, b) | Or (a, b) ->
        Kleene.and_ (is_representable a) (is_representable b)
    | Constraint.Not e -> is_representable e
    | Constraint.Cmp c -> D.is_representable c

  let eval = function
    | Leaf n -> D.eval n
    | Union {envelopp; _} -> D.eval envelopp

  (* helper that folds over tree *)
  let fold f acc =
    let rec fold acc = function
      | Leaf n -> f acc n
      | Union {sons; _} -> List.fold_left fold acc sons
    in
    fold acc

  let rec print fmt = function
    | Leaf d -> D.print fmt d
    | Union {sons; _} ->
        Format.fprintf fmt "(%a)"
          (Format.pp_print_list
             ~pp_sep:(fun f () -> Format.fprintf f " || ")
             print )
          sons

  let map f t =
    let rec loop = function
      | Leaf l -> Leaf (f l)
      | Union {envelopp; sons} ->
          Union {envelopp= f envelopp; sons= List.map loop sons}
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

  let union_list envelopp sons = Union {envelopp; sons}

  (* TODO: improve when exact join is met *)
  let join a b =
    let hull, _exact = D.join (bounding a) (bounding b) in
    (Union {envelopp= hull; sons= [a; b]}, true)

  (* TODO: improve when exact join is met *)
  let join_list l =
    let hull, _exact = D.join_list (List.map bounding l) in
    (Union {envelopp= hull; sons= l}, true)

  let meet q1 q2 =
    match (q1, q2) with
    | Leaf e1, Leaf e2 -> Option.map leaf (D.meet e1 e2)
    | Union {envelopp; sons}, b | b, Union {envelopp; sons} ->
        Option.map
          (fun _ ->
            ignore sons ;
            assert false )
          (D.meet envelopp (bounding b))

  (* Tools.join_bot2 (fun a b -> fst (join a b)) (meet b l) (meet b r) ) *)

  let meet_env hull = function
    | Leaf e -> Option.map leaf (D.meet e hull)
    | Union ({envelopp; _} as u) ->
        D.meet envelopp hull
        |> Option.map (fun b' -> Union {u with envelopp= b'})

  (* filter for numeric predicates *)
  let filter_cmp (t : t) cmp : t Consistency.t =
    match t with
    | Leaf e -> Consistency.map leaf (D.filter e cmp)
    | Union {envelopp; sons} ->
        D.filter envelopp cmp
        |> Consistency.bind (fun e' x ->
               let sons' =
                 List.fold_left
                   (fun acc e ->
                     match meet_env e' e with None -> acc | Some e -> e :: acc
                     )
                   [] sons
               in
               match sons' with
               | [] -> Unsat
               | e -> Filtered (union_list e' e, x) )

  (* filter for numeric predicates *)
  let filter_cmp_diff (t : t) cmp : (t * Tools.VarSet.t) Consistency.t =
    match t with
    | Leaf e ->
        Consistency.map (fun (d, t) -> (leaf d, t)) (D.filter_diff e cmp)
    | Union {envelopp; sons} ->
        D.filter_diff envelopp cmp
        |> Consistency.bind (fun (e', v) x ->
               let sons' =
                 List.fold_left
                   (fun acc e ->
                     match meet_env e' e with None -> acc | Some e -> e :: acc
                     )
                   [] sons
               in
               match sons' with
               | [] -> Unsat
               | e -> Filtered ((union_list e' e, v), x) )

  (* filter for boolean expressions *)
  let filter (n : t) c : (t * internal_constr) Consistency.t =
    let open Constraint in
    let rec collect_or = function
      | Or (b1, b2) -> collect_or b1 @ collect_or b2
      | b -> [b]
    in
    let rec loop e = function
      | Cmp a as c -> Consistency.map (fun x -> (x, c)) (filter_cmp e a)
      | Or _ as x -> (
          let atoms = collect_or x in
          try
            List.fold_left
              (fun acc c ->
                match acc with
                | Sat -> raise Exit
                | Unsat -> loop e c
                | Filtered ((n1, b1'), sat1) as x -> (
                  match loop e c with
                  | Sat -> raise Exit
                  | Unsat -> x
                  | Filtered ((n2, b2'), sat2) ->
                      let union, _exact = join n1 n2 in
                      Filtered ((union, Or (b1', b2')), sat1 && sat2)
                  | Pruned _ -> failwith "" )
                | Pruned _ -> failwith "" )
              (loop e (List.hd atoms))
              (List.tl atoms)
          with Exit -> Sat )
      | And (b1, b2) -> (
        match loop e b1 with
        | Unsat -> Unsat
        | Sat -> loop e b2
        | Filtered ((n1, b1'), sat1) as x -> (
          match loop n1 b2 with
          | Sat -> x
          | Unsat -> Unsat
          | Filtered ((num', b2'), sat2) ->
              Filtered ((num', And (b1', b2')), sat1 && sat2)
          | Pruned _ -> failwith "" )
        | Pruned _ -> failwith "" )
      | Not _ -> assert false
    in
    loop n c

  (* filter for boolean expressions *)
  let filter_diff (n : t) c :
      (t * internal_constr * Tools.VarSet.t) Consistency.t =
    let open Constraint in
    let rec collect_or = function
      | Or (b1, b2) -> collect_or b1 @ collect_or b2
      | b -> [b]
    in
    let rec loop e = function
      | Cmp a as c ->
          Consistency.map (fun (x, v) -> (x, c, v)) (filter_cmp_diff e a)
      | Or _ as x -> (
          let atoms = collect_or x in
          try
            List.fold_left
              (fun acc c ->
                match acc with
                | Sat -> raise Exit
                | Unsat -> loop e c
                | Filtered ((n1, b1', v1), sat1) as x -> (
                  match loop e c with
                  | Sat -> raise Exit
                  | Unsat -> x
                  | Filtered ((n2, b2', v2), sat2) ->
                      let union, _exact = join n1 n2 in
                      Filtered
                        ( (union, Or (b1', b2'), Tools.VarSet.union v1 v2)
                        , sat1 && sat2 )
                  | Pruned _ -> failwith "" )
                | Pruned _ -> failwith "" )
              (loop e (List.hd atoms))
              (List.tl atoms)
          with Exit -> Sat )
      | And (b1, b2) -> (
        match loop e b1 with
        | Unsat -> Unsat
        | Sat -> loop e b2
        | Filtered ((n1, b1', v1), sat1) as x -> (
          match loop n1 b2 with
          | Sat -> x
          | Unsat -> Unsat
          | Filtered ((num', b2', v2), sat2) ->
              Filtered
                ((num', And (b1', b2'), Tools.VarSet.union v1 v2), sat1 && sat2)
          | Pruned _ -> failwith "" )
        | Pruned _ -> failwith "" )
      | Not _ -> assert false
    in
    loop n c

  let split_along ?prec var = function
    | Leaf x -> List.rev_map leaf (D.split_along var ?prec x)
    | Union {sons; _} -> sons

  let split ?prec = function
    | Leaf x -> List.rev_map leaf (D.split ?prec x)
    | Union {sons; _} -> sons

  let split_diff ?prec = function
    | Leaf x ->
        let split_x, diff = D.split_diff ?prec x in
        (List.rev_map leaf split_x, diff)
    | Union {sons; _} -> (sons, Tools.VarSet.empty)

  let is_abstraction t i =
    try
      fold (fun () n -> if D.is_abstraction n i then raise Exit) () t ;
      false
    with Exit -> true

  (* fast overapprox *)
  let volume = function Leaf x | Union {envelopp= x; _} -> D.volume x

  let rec spawn = function
    | Leaf l -> D.spawn l
    | Union {sons; _} -> spawn (Tools.list_pick sons)

  let diff = None

  let rec to_constraint = function
    | Leaf d -> D.to_constraint d
    | Union {sons= h :: tl; _} ->
        List.fold_left
          (fun acc e -> Constraint.or_ acc (to_constraint e))
          (to_constraint h) tl
    | Union {sons= []; _} ->
        failwith "broken invariant: uniontree.sons can not be empty"

  let render t =
    match fold (fun acc e -> e :: acc) [] t with
    | h :: tl ->
        List.fold_left
          (fun acc e -> Picasso.Drawable.union acc (D.render e))
          (D.render h) tl
    | _ -> failwith "cannot render bottom element"
end
