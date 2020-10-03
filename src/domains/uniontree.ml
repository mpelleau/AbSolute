open Signature
open Bot
open Consistency
open Csp

(* This module implements the tree abstract domain described in
   Abstract Domains for Constraint Programming with Differential Equations *)

(* lifts a numeric domain to a boolean one *)
module Make (D:Numeric) : Domain = struct

  type t = Leaf of D.t
         | Union of {envelopp:D.t ; sons: t * t}

  type internal_constr = D.internal_constr Csp.boolean

  let internalize ?elem x =
    match elem with
    | Some(Leaf n) -> Csp_helper.map_constr (D.internalize ~elem:n) x
    | _ -> failwith "of_comparison should be called on a leaf"

  let externalize = Csp_helper.map_constr D.externalize

  let rec is_representable = function
    | And(a, b) | Or(a,b)->
       Kleene.and_kleene (is_representable a) (is_representable b)
    | Not(e) -> is_representable e
    | Cmp c -> D.is_representable c

  let forward_eval _ = failwith "uniontree.fwd_eval"

  (* helper that folds over tree *)
  let fold f acc =
    let rec fold acc = function
      | Leaf n -> f acc n
      | Union {sons=l,r; _ } -> fold (fold acc l) r
    in fold acc

  let print _ = failwith "print tree"

  let map f t =
    let rec loop = function
      | Leaf l -> Leaf (f l)
      | Union {envelopp;sons=l,r} ->
         Union {envelopp=f envelopp; sons= loop l,loop r}
    in
    loop t

  let add_var t v = map (fun n -> D.add_var n v) t

  let rm_var t v = map (fun n -> D.rm_var n v) t

  (* all elements defined on the same set of variables *)
  let vars = function
    | Leaf e
    | Union {envelopp = e;_} -> D.vars e

  let empty = Leaf D.empty

  let bounding = function
    | Leaf n -> n
    | Union {envelopp;_} -> envelopp

  (* constructors *)
  let leaf x = Leaf x
  let union e a b = Union {envelopp=e; sons = a,b}

  (* TODO: improve when exact join is met *)
  let join a b =
    Union {envelopp=fst (D.join (bounding a) (bounding b)); sons=a,b}, true

   let rec meet q1 q2 =
    match q1,q2 with
    | Leaf e1, Leaf e2 -> Bot.lift_bot (fun x -> Leaf x) (D.meet e1 e2)
    | Union {envelopp; sons=l,r}, b | b, Union {envelopp; sons=l,r}->
       (match D.meet envelopp (bounding b) with
        | Bot -> Bot
        | Nb _ -> Bot.join_bot2 (fun a b -> join a b |> fst) (meet b l) (meet b r))

  let meet_env hull = function
    | Leaf e ->  Bot.lift_bot (fun x -> Leaf x) (D.meet e hull)
    | Union ({envelopp;_} as u) ->
       match D.meet envelopp hull with
       | Bot -> Bot
       | Nb b' -> Nb (Union {u with envelopp=b'})

  (* computes the list of succesfully filtered element and the rest *)
  let filter (q:t) a : t Consistency.t =
    match q with
    | Leaf e -> Consistency.map leaf (D.filter e a)
    | Union {envelopp; sons=l,r} ->
       match D.filter envelopp a with
       | Unsat -> Unsat
       | Sat -> Sat
       | Filtered (e',x) ->
          (match Bot.join_bot2 (union e') (meet_env e' l) (meet_env e' r) with
           | Bot -> Unsat
           | Nb e -> Filtered (e,x))

  let filter (num:t) c : t Consistency.t =
    let rec loop num c =
      match c with
      | Cmp a -> filter num a
      | Or (b1,b2) ->
         (match loop num b1 with
          | Sat -> Sat
          | Unsat -> loop num b2
          | Filtered (n1,_) as x ->
             (match loop num b2 with
              | Sat -> Sat
              | Unsat -> x
              | Filtered (n2,_) ->
                 let union,exact = join n1 n2 in
                 Filtered ((union,exact))))
      | And(b1,b2) -> Consistency.fold_and loop num [b1;b2]
      | Not _ -> assert false
    in loop num c

  let split = function
    | Leaf x -> List.map (fun x -> Leaf x) (D.split x)
    | Union {sons=l,r;_} -> [l;r]

  let is_abstraction t i =
    try
      fold (fun () n -> if D.is_abstraction n i then raise Exit) () t;
      false
    with Exit -> true

  (* fast overapprox *)
  let volume = function
    | Leaf x | Union {envelopp=x;_} -> D.volume x

  let rec spawn = function
    | Leaf l -> D.spawn l
    | Union {sons=l,_; _} -> spawn l

  let prune = None

  let rec to_bexpr = function
    | Leaf d -> D.to_bexpr d
    | Union {sons=l,r; _} -> Csp.Or((to_bexpr l), (to_bexpr r))

  let render t =
    match fold (fun acc e -> e::acc) [] t with
    | h::tl -> List.fold_left (fun acc e -> Picasso.Drawable.union acc
                                              (D.render e))
                 (D.render h) tl
    | _ -> failwith "cannot render bottom element"
end
