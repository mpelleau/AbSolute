(*  
  This file is derived from the map.ml file from the OCaml distribution.
  Changes are marked with the [AM] symbol.
  Based on rev. 10468 2010-05-25 13:29:43Z

  Author of the changes: Antoine Mine, 2014

  Original copyright follows.
*)

(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

module type OrderedType =
  sig
    type t
    val compare: t -> t -> int
  end

module type S =
  sig
    type key
    type +'a t
    val empty: 'a t
    val is_empty: 'a t -> bool
    val mem:  key -> 'a t -> bool
    val add: key -> 'a -> 'a t -> 'a t
    val singleton: key -> 'a -> 'a t
    val remove: key -> 'a t -> 'a t
    val merge: (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter: (key -> 'a -> unit) -> 'a t -> unit
    val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all: (key -> 'a -> bool) -> 'a t -> bool
    val exists: (key -> 'a -> bool) -> 'a t -> bool
    val filter: (key -> 'a -> bool) -> 'a t -> 'a t
    val partition: (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal: 'a t -> int
    val bindings: 'a t -> (key * 'a) list
    val min_binding: 'a t -> (key * 'a)
    val max_binding: 'a t -> (key * 'a)
    val choose: 'a t -> (key * 'a)
    val split: key -> 'a t -> 'a t * 'a option * 'a t
    val find: key -> 'a t -> 'a
    val map: ('a -> 'b) -> 'a t -> 'b t
    val mapi: (key -> 'a -> 'b) -> 'a t -> 'b t


    (* [AM] additions by Antoine Mine' *)

    val of_list: (key * 'a) list -> 'a t

    val map2: (key -> 'a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
    val iter2: (key -> 'a -> 'b -> unit) -> 'a t -> 'b t -> unit
    val fold2: (key -> 'a -> 'b -> 'c -> 'c) -> 'a t -> 'b t -> 'c -> 'c
    val for_all2: (key -> 'a -> 'b -> bool) -> 'a t -> 'b t -> bool
    val exists2: (key -> 'a -> 'b -> bool) -> 'a t -> 'b t -> bool

    val map2z: (key -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
    val iter2z: (key -> 'a -> 'a -> unit) -> 'a t -> 'a t -> unit
    val fold2z: (key -> 'a -> 'a -> 'b -> 'b) -> 'a t -> 'a t -> 'b -> 'b
    val for_all2z: (key -> 'a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val exists2z: (key -> 'a -> 'a -> bool) -> 'a t -> 'a t -> bool

    val map2o: (key -> 'a -> 'c) -> (key -> 'b -> 'c) -> (key -> 'a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
    val iter2o: (key -> 'a -> unit) -> (key -> 'b -> unit) -> (key -> 'a -> 'b -> unit) -> 'a t -> 'b t -> unit
    val fold2o: (key -> 'a -> 'c -> 'c) -> (key -> 'b -> 'c -> 'c) -> (key -> 'a -> 'b -> 'c -> 'c) -> 'a t -> 'b t -> 'c -> 'c
    val for_all2o: (key -> 'a -> bool) -> (key -> 'b -> bool) -> (key -> 'a -> 'b -> bool) -> 'a t -> 'b t -> bool
    val exists2o: (key -> 'a -> bool) -> (key -> 'b -> bool) -> (key -> 'a -> 'b -> bool) -> 'a t -> 'b t -> bool

    val map2zo: (key -> 'a -> 'a) -> (key -> 'a -> 'a) -> (key -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
    val iter2zo: (key -> 'a -> unit) -> (key -> 'a -> unit) -> (key -> 'a -> 'a -> unit) -> 'a t -> 'a t -> unit
    val fold2zo: (key -> 'a -> 'b -> 'b) -> (key -> 'a -> 'b -> 'b) -> (key -> 'a -> 'a -> 'b -> 'b) -> 'a t -> 'a t -> 'b -> 'b
    val for_all2zo: (key -> 'a -> bool) -> (key -> 'a -> bool) -> (key -> 'a -> 'a -> bool) -> 'a t -> 'a t -> bool
     val exists2zo: (key -> 'a -> bool) -> (key -> 'a -> bool) -> (key -> 'a -> 'a -> bool) -> 'a t -> 'a t -> bool

    val map_slice: (key -> 'a -> 'a) -> 'a t -> key -> key -> 'a t
    val iter_slice: (key -> 'a -> unit) -> 'a t -> key -> key -> unit
    val fold_slice: (key -> 'a -> 'b -> 'b) -> 'a t -> key -> key -> 'b -> 'b
    val for_all_slice: (key -> 'a -> bool) -> 'a t -> key -> key -> bool
    val exists_slice: (key -> 'a -> bool) -> 'a t -> key -> key -> bool

    val key_equal: 'a t -> 'a t -> bool
    val key_subset: 'a t -> 'a t -> bool

    val find_greater: key -> 'a t -> key * 'a
    val find_less: key -> 'a t -> key * 'a
    val find_greater_equal: key -> 'a t -> key * 'a
    val find_less_equal: key -> 'a t -> key * 'a

  end

module Make(Ord: OrderedType) = (struct

    type key = Ord.t

    type 'a t =
        Empty
      | Node of 'a t * key * 'a * 'a t * int

    let height = function
        Empty -> 0
      | Node(_,_,_,_,h) -> h

    let create l x d r =
      let hl = height l and hr = height r in
      Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

    let singleton x d = Node(Empty, x, d, Empty, 1)

    let bal l x d r =
      let hl = match l with Empty -> 0 | Node(_,_,_,_,h) -> h in
      let hr = match r with Empty -> 0 | Node(_,_,_,_,h) -> h in
      if hl > hr + 2 then begin
        match l with
          Empty -> invalid_arg "Mapext.bal"
        | Node(ll, lv, ld, lr, _) ->
            if height ll >= height lr then
              create ll lv ld (create lr x d r)
            else begin
              match lr with
                Empty -> invalid_arg "Mapext.bal"
              | Node(lrl, lrv, lrd, lrr, _)->
                  create (create ll lv ld lrl) lrv lrd (create lrr x d r)
            end
      end else if hr > hl + 2 then begin
        match r with
          Empty -> invalid_arg "Mapext.bal"
        | Node(rl, rv, rd, rr, _) ->
            if height rr >= height rl then
              create (create l x d rl) rv rd rr
            else begin
              match rl with
                Empty -> invalid_arg "Mapext.bal"
              | Node(rll, rlv, rld, rlr, _) ->
                  create (create l x d rll) rlv rld (create rlr rv rd rr)
            end
      end else
        Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

    let empty = Empty

    let is_empty = function Empty -> true | _ -> false

    let rec add x data = function
        Empty ->
          Node(Empty, x, data, Empty, 1)
      | Node(l, v, d, r, h) ->
          let c = Ord.compare x v in
          if c = 0 then
            Node(l, x, data, r, h)
          else if c < 0 then
            bal (add x data l) v d r
          else
            bal l v d (add x data r)

    let rec find x = function
        Empty ->
          raise Not_found
      | Node(l, v, d, r, _) ->
          let c = Ord.compare x v in
          if c = 0 then d
          else find x (if c < 0 then l else r)

    let rec mem x = function
        Empty ->
          false
      | Node(l, v, d, r, _) ->
          let c = Ord.compare x v in
          c = 0 || mem x (if c < 0 then l else r)

    let rec min_binding = function
        Empty -> raise Not_found
      | Node(Empty, x, d, r, _) -> (x, d)
      | Node(l, x, d, r, _) -> min_binding l

    let rec max_binding = function
        Empty -> raise Not_found
      | Node(l, x, d, Empty, _) -> (x, d)
      | Node(l, x, d, r, _) -> max_binding r

    let rec remove_min_binding = function
        Empty -> invalid_arg "Mapext.remove_min_elt"
      | Node(Empty, x, d, r, _) -> r
      | Node(l, x, d, r, _) -> bal (remove_min_binding l) x d r

    let merge t1 t2 =
      match (t1, t2) with
        (Empty, t) -> t
      | (t, Empty) -> t
      | (_, _) ->
          let (x, d) = min_binding t2 in
          bal t1 x d (remove_min_binding t2)

    let rec remove x = function
        Empty ->
          Empty
      | Node(l, v, d, r, h) ->
          let c = Ord.compare x v in
          if c = 0 then
            merge l r
          else if c < 0 then
            bal (remove x l) v d r
          else
            bal l v d (remove x r)

    let rec iter f = function
        Empty -> ()
      | Node(l, v, d, r, _) ->
          iter f l; f v d; iter f r

    let rec map f = function
        Empty ->
          Empty
      | Node(l, v, d, r, h) ->
          let l' = map f l in
          let d' = f d in
          let r' = map f r in
          Node(l', v, d', r', h)

    let rec mapi f = function
        Empty ->
          Empty
      | Node(l, v, d, r, h) ->
          let l' = mapi f l in
          let d' = f v d in
          let r' = mapi f r in
          Node(l', v, d', r', h)

    let rec fold f m accu =
      match m with
        Empty -> accu
      | Node(l, v, d, r, _) ->
          fold f r (f v d (fold f l accu))

    (* [AM] changed to call p in the key order *)
    let rec for_all p = function
        Empty -> true
      | Node(l, v, d, r, _) -> for_all p l && p v d && for_all p r

    (* [AM] changed to call p in the key order *)
    let rec exists p = function
        Empty -> false
      | Node(l, v, d, r, _) -> exists p l || p v d || exists p r

    (* [AM] changed to call p in the key order *)
    let filter p s =
      fold (fun k d a -> if p k d then add k d a else a) s Empty

    let partition p s =
      let rec part (t, f as accu) = function
        | Empty -> accu
        | Node(l, v, d, r, _) ->
            part (part (if p v d then (add v d t, f) else (t, add v d f)) l) r in
      part (Empty, Empty) s

    (* Same as create and bal, but no assumptions are made on the
       relative heights of l and r. *)

    let rec join l v d r =
      match (l, r) with
        (Empty, _) -> add v d r
      | (_, Empty) -> add v d l
      | (Node(ll, lv, ld, lr, lh), Node(rl, rv, rd, rr, rh)) ->
          if lh > rh + 2 then bal ll lv ld (join lr v d r) else
          if rh > lh + 2 then bal (join l v d rl) rv rd rr else
          create l v d r

    (* Merge two trees l and r into one.
       All elements of l must precede the elements of r.
       No assumption on the heights of l and r. *)

    let concat t1 t2 =
      match (t1, t2) with
        (Empty, t) -> t
      | (t, Empty) -> t
      | (_, _) ->
          let (x, d) = min_binding t2 in
          join t1 x d (remove_min_binding t2)

    let concat_or_join t1 v d t2 =
      match d with
      | Some d -> join t1 v d t2
      | None -> concat t1 t2

    let rec split x = function
        Empty ->
          (Empty, None, Empty)
      | Node(l, v, d, r, _) ->
          let c = Ord.compare x v in
          if c = 0 then (l, Some d, r)
          else if c < 0 then
            let (ll, pres, rl) = split x l in (ll, pres, join rl v d r)
          else
            let (lr, pres, rr) = split x r in (join l v d lr, pres, rr)

    let rec merge f s1 s2 =
      match (s1, s2) with
        (Empty, Empty) -> Empty
      | (Node (l1, v1, d1, r1, h1), _) when h1 >= height s2 ->
          let (l2, d2, r2) = split v1 s2 in
          concat_or_join (merge f l1 l2) v1 (f v1 (Some d1) d2) (merge f r1 r2)
      | (_, Node (l2, v2, d2, r2, h2)) ->
          let (l1, d1, r1) = split v2 s1 in
          concat_or_join (merge f l1 l2) v2 (f v2 d1 (Some d2)) (merge f r1 r2)
      | _ ->
          assert false

    type 'a enumeration = End | More of key * 'a * 'a t * 'a enumeration

    let rec cons_enum m e =
      match m with
        Empty -> e
      | Node(l, v, d, r, _) -> cons_enum l (More(v, d, r, e))

    let compare cmp m1 m2 =
      let rec compare_aux e1 e2 =
          match (e1, e2) with
          (End, End) -> 0
        | (End, _)  -> -1
        | (_, End) -> 1
        | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
            let c = Ord.compare v1 v2 in
            if c <> 0 then c else
            let c = cmp d1 d2 in
            if c <> 0 then c else
            compare_aux (cons_enum r1 e1) (cons_enum r2 e2)
      in compare_aux (cons_enum m1 End) (cons_enum m2 End)

    let equal cmp m1 m2 =
      let rec equal_aux e1 e2 =
          match (e1, e2) with
          (End, End) -> true
        | (End, _)  -> false
        | (_, End) -> false
        | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
            Ord.compare v1 v2 = 0 && cmp d1 d2 &&
            equal_aux (cons_enum r1 e1) (cons_enum r2 e2)
      in equal_aux (cons_enum m1 End) (cons_enum m2 End)

    let rec cardinal = function
        Empty -> 0
      | Node(l, _, _, r, _) -> cardinal l + 1 + cardinal r

    let rec bindings_aux accu = function
        Empty -> accu
      | Node(l, v, d, r, _) -> bindings_aux ((v, d) :: bindings_aux accu r) l

    let bindings s =
      bindings_aux [] s

    let choose = min_binding



    (* [AM] additions by Antoine Mine' *)
    (* ******************************* *)


    let of_list l =
      List.fold_left (fun acc (k,x) -> add k x acc) empty l


    (* similar to split, but returns unbalanced trees *)
    let rec cut k = function
      Empty -> Empty,None,Empty
    | Node (l1,k1,d1,r1,h1) ->
        let c = Ord.compare k k1 in
        if c < 0 then 
          let l2,d2,r2 = cut k l1 in (l2,d2,Node (r2,k1,d1,r1,h1))
        else if c > 0 then
          let l2,d2,r2 = cut k r1 in (Node (l1,k1,d1,l2,h1),d2,r2)
        else (l1,Some d1,r1)


    (* binary operations that fail on maps with different keys *)

    (* functions are called in increasing key order *)

    let rec map2 f m1 m2 =
      match m1 with
      | Empty -> if m2 = Empty then Empty else invalid_arg "Mapext.map2"
      | Node (l1,k,d1,r1,h1) ->
          match cut k m2 with 
          | l2, Some d2, r2 ->
              Node (map2 f l1 l2, k, f k d1 d2, map2 f r1 r2, h1)
          | _, None, _ -> invalid_arg "Mapext.map2"
            
    let rec iter2 f m1 m2 =
      match m1 with
      | Empty -> if m2 = Empty then () else invalid_arg "Mapext.iter2"
      | Node (l1,k,d1,r1,h1) ->
          match cut k m2 with 
          | l2, Some d2, r2 -> iter2 f l1 l2; f k d1 d2; iter2 f r1 r2
          | _, None, _ -> invalid_arg "Mapext.iter2"
            
    let rec fold2 f m1 m2 acc =
      match m1 with
      | Empty -> if m2 = Empty then acc else invalid_arg "Mapext.fold2"
      | Node (l1,k,d1,r1,h1) ->
          match cut k m2 with 
          | l2, Some d2, r2 -> 
              fold2 f r1 r2 (f k d1 d2 (fold2 f l1 l2 acc))
          | _, None, _ -> invalid_arg "Mapext.fold2"
            
    let rec for_all2 f m1 m2 =
      match m1 with
      | Empty -> if m2 = Empty then true else invalid_arg "Mapext.for_all2"
      | Node (l1,k,d1,r1,h1) ->
          match cut k m2 with 
          | l2, Some d2, r2 -> 
              for_all2 f l1 l2 && f k d1 d2 && for_all2 f r1 r2
          | _, None, _ -> invalid_arg "Mapext.for_all2"
            
    let rec exists2 f m1 m2 =
      match m1 with
      | Empty -> if m2 = Empty then false else invalid_arg "Mapext.exists2"
      | Node (l1,k,d1,r1,h1) ->
          match cut k m2 with 
          | l2, Some d2, r2 -> 
              exists2 f l1 l2 || f k d1 d2 || exists2 f r1 r2
          | _, None, _ -> invalid_arg "Mapext.exists2"
            

    (* as above, but ignore physically equal subtrees
       - for map, assumes: f k d d = d
       - for iter, assumes: f k d d has no effect
       - for fold, assumes: k f d d acc = acc
       - for for_all, assumes: f k d d = true
       - for exists, assumes: f k d d = false
     *)

    let rec map2z f m1 m2 =
      if m1 == m2 then m1 else
      match m1 with
      | Empty -> if m2 = Empty then Empty else invalid_arg "Mapext.map2z"
      | Node (l1,k,d1,r1,h1) ->
          match cut k m2 with 
          | l2, Some d2, r2 ->
              let d = if d1 == d2 then d1 else f k d1 d2 in
              Node (map2z f l1 l2, k, d, map2z f r1 r2, h1)
          | _, None, _ -> invalid_arg "Mapext.map2z"
            
    let rec iter2z f m1 m2 =
      if m1 == m2 then () else
      match m1 with
      | Empty -> if m2 = Empty then () else invalid_arg "Mapext.iter2z"
      | Node (l1,k,d1,r1,h1) ->
          match cut k m2 with 
          | l2, Some d2, r2 -> 
              iter2z f l1 l2; (if d1 != d2 then f k d1 d2); iter2z f r1 r2
          | _, None, _ -> invalid_arg "Mapext.iter2z"
            
    let rec fold2z f m1 m2 acc =
      if m1 == m2 then acc else
      match m1 with
      | Empty -> if m2 = Empty then acc else invalid_arg "Mapext.fold2z"
      | Node (l1,k,d1,r1,h1) ->
          match cut k m2 with 
          | l2, Some d2, r2 -> 
              let acc = fold2z f l1 l2 acc in
              let acc = if d1 == d2 then acc else f k d1 d2 acc in
              fold2z f r1 r2 acc
          | _, None, _ -> invalid_arg "Mapext.fold2z"
            
    let rec for_all2z f m1 m2 =
      (m1 == m2) ||
      (match m1 with
      | Empty -> if m2 = Empty then true else invalid_arg "Mapext.for_all2z"
      | Node (l1,k,d1,r1,h1) ->
          match cut k m2 with 
          | l2, Some d2, r2 -> 
              (for_all2z f l1 l2) && 
              (d1 == d2 || f k d1 d2) && 
              (for_all2z f r1 r2)
          | _, None, _ -> invalid_arg "Mapext.for_all2z"
      )     
            
    let rec exists2z f m1 m2 =
      (m1 != m2) &&
      (match m1 with
      | Empty -> if m2 = Empty then false else invalid_arg "Mapext.exists2z"
      | Node (l1,k,d1,r1,h1) ->
          match cut k m2 with 
          | l2, Some d2, r2 -> 
              (exists2z f l1 l2) ||
              (d1 != d2 && f k d1 d2) ||
              (exists2z f r1 r2)
          | _, None, _ -> invalid_arg "Mapext.exists2z"
      )     


    (* as above, but allow maps with different keys *)

    let rec map2o f1 f2 f m1 m2 =
      match m1 with
      | Empty -> mapi f2 m2
      | Node (l1,k,d1,r1,h1) ->
          let l2, d2, r2 = cut k m2 in
          let l = map2o f1 f2 f l1 l2 in
          let d = match d2 with None -> f1 k d1 | Some d2 -> f k d1 d2 in
          let r = map2o f1 f2 f r1 r2 in
          join l k d r
            
    let rec iter2o f1 f2 f m1 m2 =
      match m1 with
      | Empty -> iter f2 m2
      | Node (l1,k,d1,r1,h1) ->
          let l2, d2, r2 = cut k m2 in
          iter2o f1 f2 f l1 l2;
          (match d2 with None -> f1 k d1 | Some d2 -> f k d1 d2);
          iter2o f1 f2 f r1 r2
            
    let rec fold2o f1 f2 f m1 m2 acc =
      match m1 with
      | Empty -> fold f2 m2 acc
      | Node (l1,k,d1,r1,h1) ->
          let l2, d2, r2 = cut k m2 in
          let acc = fold2o f1 f2 f l1 l2 acc in
          let acc = match d2 with 
          | None -> f1 k d1 acc | Some d2 -> f k d1 d2 acc
          in
          fold2o f1 f2 f r1 r2 acc
            
    let rec for_all2o f1 f2 f m1 m2 =
      match m1 with
      | Empty -> for_all f2 m2 
      | Node (l1,k,d1,r1,h1) ->
          let l2, d2, r2 = cut k m2 in
          (for_all2o f1 f2 f l1 l2) &&
          (match d2 with None -> f1 k d1 | Some d2 -> f k d1 d2) &&
          (for_all2o f1 f2 f r1 r2)

    let rec exists2o f1 f2 f m1 m2 =
      match m1 with
      | Empty -> exists f2 m2 
      | Node (l1,k,d1,r1,h1) ->
          let l2, d2, r2 = cut k m2 in
          (exists2o f1 f2 f l1 l2) ||
          (match d2 with None -> f1 k d1 | Some d2 -> f k d1 d2) ||
          (exists2o f1 f2 f r1 r2)


    (* all together now *)

    let rec map2zo f1 f2 f m1 m2 =
      if m1 == m2 then m1 else
      match m1 with
      | Empty -> mapi f2 m2
      | Node (l1,k,d1,r1,h1) ->
          let l2, d2, r2 = cut k m2 in
          let l = map2zo f1 f2 f l1 l2 in
          let d = match d2 with 
          | None -> f1 k d1 
          | Some d2 -> if d1 == d2 then d1 else f k d1 d2 
          in
          let r = map2zo f1 f2 f r1 r2 in
          join l k d r
            
    let rec iter2zo f1 f2 f m1 m2 =
      if m1 == m2 then () else
      match m1 with
      | Empty -> iter f2 m2
      | Node (l1,k,d1,r1,h1) ->
          let l2, d2, r2 = cut k m2 in
          iter2zo f1 f2 f l1 l2;
          (match d2 with 
          | None -> f1 k d1 
          | Some d2 -> if d1 != d2 then f k d1 d2);
          iter2zo f1 f2 f r1 r2
            
    let rec fold2zo f1 f2 f m1 m2 acc =
      if m1 == m2 then acc else
      match m1 with
      | Empty -> fold f2 m2 acc
      | Node (l1,k,d1,r1,h1) ->
          let l2, d2, r2 = cut k m2 in
          let acc = fold2zo f1 f2 f l1 l2 acc in
          let acc = match d2 with 
          | None -> f1 k d1 acc 
          | Some d2 -> if d1 == d2 then acc else f k d1 d2 acc
          in
          fold2zo f1 f2 f r1 r2 acc
            
    let rec for_all2zo f1 f2 f m1 m2 =
      (m1 == m2) ||
      (match m1 with
      | Empty -> for_all f2 m2 
      | Node (l1,k,d1,r1,h1) ->
          let l2, d2, r2 = cut k m2 in
          (for_all2zo f1 f2 f l1 l2) &&
          (match d2 with None -> f1 k d1 | Some d2 -> d1 == d2 || f k d1 d2) &&
          (for_all2zo f1 f2 f r1 r2)
      )

    let rec exists2zo f1 f2 f m1 m2 =
      (m1 != m2) &&
      (match m1 with
      | Empty -> exists f2 m2 
      | Node (l1,k,d1,r1,h1) ->
          let l2, d2, r2 = cut k m2 in
          (exists2zo f1 f2 f l1 l2) ||
          (match d2 with None -> f1 k d1 | Some d2 -> d1 != d2 && f k d1 d2) ||
          (exists2zo f1 f2 f r1 r2)
      )


    (* iterators limited to keys between two bounds *)

    let rec map_slice f m lo hi =
      match m with
      | Empty -> Empty
      | Node (l,k,d,r,h) ->
          let c1, c2 = Ord.compare k lo, Ord.compare k hi in
          let l = if c1 > 0 then map_slice f l lo k else l in
          let d = if c1 >= 0 && c2 <= 0 then f k d else d in
          let r = if c2 < 0 then map_slice f r k hi else r in
          Node (l,k,d,r,h)

    let rec iter_slice f m lo hi =
      match m with
      | Empty -> ()
      | Node (l,k,d,r,_) ->
          let c1, c2 = Ord.compare k lo, Ord.compare k hi in
          if c1 > 0 then iter_slice f l lo k;
          if c1 >= 0 && c2 <= 0 then f k d;
          if c2 < 0 then iter_slice f r k hi

    let rec fold_slice f m lo hi acc =
      match m with
      | Empty -> acc
      | Node (l,k,d,r,_) ->
          let c1, c2 = Ord.compare k lo, Ord.compare k hi in
          let acc = if c1 > 0 then fold_slice f l lo k acc else acc in
          let acc = if c1 >= 0 && c2 <= 0 then f k d acc else acc in
          if c2 < 0 then fold_slice f r k hi acc else acc

    let rec for_all_slice f m lo hi =
      match m with
      | Empty -> true
      | Node (l,k,d,r,_) ->
          let c1, c2 = Ord.compare k lo, Ord.compare k hi in
          (c1 <= 0 || for_all_slice f l lo k) &&
          (c1 < 0 || c2 > 0 || f k d) &&
          (c2 >= 0 || for_all_slice f r k hi)

    let rec exists_slice f m lo hi =
      match m with
      | Empty -> false
      | Node (l,k,d,r,_) ->
          let c1, c2 = Ord.compare k lo, Ord.compare k hi in
          (c1 > 0 && exists_slice f l lo k) ||
          (c1 >= 0 && c2 <= 0 && f k d) ||
          (c2 < 0 && exists_slice f r k hi)


    (* key set comparison *)

    let rec key_equal m1 m2 =
      (m1 == m2) || 
      (match m1 with
      | Empty -> m2 = Empty
      | Node (l1, k, _, r1, _) ->
          match cut k m2 with
          | _, None, _ -> false
          | l2, Some _, r2 -> key_equal l1 l2 && key_equal r1 r2
      )

    let rec key_subset m1 m2 =
      (m1 == m2) || 
      (match m1 with
      | Empty -> true
      | Node (l1, k, _, r1, _) ->
          match cut k m2 with
          | _, None, _ -> false
          | l2, Some _, r2 -> key_subset l1 l2 && key_subset r1 r2
      )


    (* nagivation *)

    let find_greater_equal k m =
      let rec aux m found = match m with
      | Empty -> (match found with None -> raise Not_found | Some x -> x)
      | Node (l, kk, d, r, _) ->
          let c = Ord.compare k kk in
          if c = 0 then kk, d else
          if c > 0 then aux r found else
          aux l (Some (kk, d))
      in
      aux m None 

    let find_greater k m =
      let rec aux m found = match m with
      | Empty -> (match found with None -> raise Not_found | Some x -> x)
      | Node (l, kk, d, r, _) ->
          let c = Ord.compare k kk in
          if c >= 0 then aux r found else
          aux l (Some (kk, d))
      in
      aux m None 

    let find_less_equal k m =
      let rec aux m found = match m with
      | Empty -> (match found with None -> raise Not_found | Some x -> x)
      | Node (l, kk, d, r, _) ->
          let c = Ord.compare k kk in
          if c = 0 then kk, d else
          if c < 0 then aux l found else
          aux r (Some (kk, d))
      in
      aux m None 

    let find_less k m =
      let rec aux m found = match m with
      | Empty -> (match found with None -> raise Not_found | Some x -> x)
      | Node (l, kk, d, r, _) ->
          let c = Ord.compare k kk in
          if c <= 0 then aux l found else
          aux r (Some (kk, d))
      in
      aux m None 


end: S with type key = Ord.t)
