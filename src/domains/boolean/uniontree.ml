open Signature
open Bot
open Consistency
open Csp

(* This module implements the tree abstract domain described in
   Abstract Domains for Constraint Programming with Differential Equations *)

module Make (D:Numeric) : Domain = struct

  type t = Leaf of D.t
         | Union of {envelopp:D.t ; sons: t * t}

  type internal_constr = D.internal_constr Csp.boolean

  let internalize ?elem c =
    match elem with
    | Some(Leaf n) ->
       let c' = Csp_helper.remove_not c in
       Csp_helper.map_constr (D.internalize ~elem:n) c'
    | _ -> failwith "of_comparison should be called on a leaf"

  let externalize = Csp_helper.map_constr D.externalize

  let rec is_representable = function
    | And(a, b) | Or(a,b)->
       Kleene.and_kleene (is_representable a) (is_representable b)
    | Not(e) -> is_representable e
    | Cmp c -> D.is_representable c

  let forward_eval = function
    | Leaf n -> D.forward_eval n
    | Union {envelopp;_} -> D.forward_eval envelopp

  (* helper that folds over tree *)
  let fold f acc =
    let rec fold acc = function
      | Leaf n -> f acc n
      | Union {sons=l,r; _ } -> fold (fold acc l) r
    in fold acc

  let rec print fmt = function
    | Leaf d -> D.print fmt d
    | Union {sons=l,r;_} -> Format.fprintf fmt "(%a || %a)" print l print r

  let map f t =
    let rec loop = function
      | Leaf l -> Leaf (f l)
      | Union {envelopp;sons=l,r} ->
         Union {envelopp=f envelopp; sons=loop l,loop r}
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

  (* TODO: improve when exact join is met *)
  let join a b =
    Union {envelopp=fst (D.join (bounding a) (bounding b)); sons=a,b}

   let rec meet q1 q2 =
     Format.printf "meet\n%!";
     match q1,q2 with
    | Leaf e1, Leaf e2 -> Bot.lift_bot leaf (D.meet e1 e2)
    | Union {envelopp; sons=l,r}, b | b, Union {envelopp; sons=l,r}->
       (match D.meet envelopp (bounding b) with
        | Bot -> Bot
        | Nb _ -> Bot.join_bot2 join (meet b l) (meet b r))

  (* filter for numeric predicates *)
  let filter_cmp (t:t) cmp : t Consistency.t =
    Format.printf "filter numeric on:\n%a and %a\n%!"
      print t Csp_printer.print_cmp (D.externalize cmp);
    let rec loop = function
    | Leaf e -> Consistency.map leaf (D.filter e cmp)
    | Union {envelopp; sons=l,r} ->
       Consistency.bind (fun _ _->
           match loop l with
           | Unsat -> loop r
           | Sat ->
              (match loop r with
               | Unsat -> Filtered(l,true)
               | Sat -> Sat
               | Filtered (r',sat) -> Filtered ((join l r'),sat))
           | Filtered(l',satl) as left ->
              (match loop r with
               | Unsat -> left
               | Sat -> Filtered ((join l' r),satl)
               | Filtered (r',satr) -> Filtered ((join l' r'),satl && satr))
         ) (D.filter envelopp cmp)
    in
    let res = loop t in
    (match res with
    | Sat -> Format.printf "result: sat \n\n";
    | Unsat -> Format.printf "result: unsat \n\n";
    | Filtered (x,s) ->  Format.printf "result: %a. sat:%b\n\n%!" print x s);
    res

  (* filter for boolean expressions *)
  let filter (num:t) c : t Consistency.t =
    let rec loop num = function
      | Cmp a -> filter_cmp num a
      | Or (b1,b2) ->
         (match loop num b1 with
          | Sat -> Sat
          | Unsat -> loop num b2
          | Filtered (n1,sat1) as x ->
             (match loop num b2 with
              | Sat -> Sat
              | Unsat -> x
              | Filtered (n2,sat2) ->
                 Filtered (( join n1 n2,sat1 && sat2))))
      | And(b1,b2) ->
         Format.printf "filtering and\n%!";
         Consistency.fold_and loop num [b1;b2]
      | Not _ -> assert false
    in loop num c

  let join a b = join a b, true

  let split = function
    | Leaf x -> List.map leaf (D.split x)
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
