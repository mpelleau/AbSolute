(* Reduced product of domains A and B where B is more expressive than A *)

open Signature
open Consistency

module Make (A : AbstractCP) (B : AbstractCP)  =
  struct

    type t = A.t * B.t

    let is_representable = B.is_representable

    let to_bexpr (a, b) = (A.to_bexpr a)@(B.to_bexpr b)

    let a_meet_b a b : B.t Consistency.t =
      let a_expr = A.to_bexpr a in
      Consistency.fold_and B.filter b a_expr

    let b_meet_a a b : A.t Consistency.t =
      let b_expr = B.to_bexpr b in
      Consistency.fold_and A.filter a b_expr

    let reduced_product (a:A.t) (b:B.t) : (A.t * B.t) Consistency.t =
      let new_a = b_meet_a a b in
      match new_a with
      | Sat -> Consistency.map (fun b -> a,b) (a_meet_b a b)
      | Unsat -> Unsat
      | Filtered (a',success1) ->
         match a_meet_b a' b with
         | Sat -> Filtered ((a',b),success1)
         | Unsat -> Unsat
         | Filtered (b',success2) -> Filtered((a',b'),success1 && success2)

    let empty = A.empty,B.empty

    let add_var (abs,abs') v = (A.add_var abs v), (B.add_var abs' v)

    let var_bounds (abs,abs') v  =
      let (la, ha) = A.var_bounds abs v
      and (lb, hb) = B.var_bounds abs' v in
      ((max la lb), (min ha hb))

    let rem_var (a,b) v = (A.rem_var a v),(B.rem_var b v)

    let bounds (abs,abs')  =
      let la = A.bounds abs
      and lb = B.bounds abs' in
      let (tmp, _) = List.split lb in
      let (same, _) = List.partition (fun (v, _) -> List.mem v tmp) la in
      let (tmp, _) = List.split same in
      let (_, diffb) = List.partition (fun (v, _) -> List.mem v tmp) lb in
      List.append la diffb

    let vars (abs, abs') =
      let va = A.vars abs
      and vb = B.vars abs' in
      List.sort_uniq (compare) (va@vb)

    let is_empty (abs, abs') = A.is_empty abs || B.is_empty abs'

    let prune : (t -> t -> t list) option = None

    let split ((abs, abs'):t) (jacobian : Csp.ctrs) =
      let split_a = A.split abs jacobian in
      List.map (fun x -> (x, abs')) split_a

    let join (a,a') (b,b') = (A.join a b), (B.join a' b')

    let meet (a,a') (b,b') =
      match reduced_product (A.meet a b) (B.meet a' b') with
      | Sat -> (a,a')
      | Unsat -> raise Bot.Bot_found
      | Filtered (x,_) -> x

    let filter ((a, b):t) ((e1, op, e2) as cons) : t Consistency.t =
      let open Kleene in
      match B.is_representable (Csp.Cmp(e1, op, e2)) with
      | True -> Consistency.map (fun b -> a,b) (B.filter b cons)
      | Unknown | False -> Consistency.map (fun a -> a,b) (A.filter a cons)

    let forward_eval ((a, b):t) obj =
      let (l_a,u_a) = A.forward_eval a obj in
      let (l_b,u_b) = B.forward_eval b obj in
      (max l_a l_b),(min u_a u_b)

    let print fmt ((abs, abs'):t) =
      Format.fprintf fmt "%a, %a" A.print abs B.print abs'

    let volume ((_, abs'):t) =
      B.volume abs'

    (* concretization function *)
    let spawn (a,b) =
      let rec generate () =
        let i = A.spawn a in
        if B.is_abstraction b i then i
        else generate ()
      in generate ()

    let is_abstraction (a,b) i =
      A.is_abstraction a i && B.is_abstraction b i

    let render (a,b) =
      Picasso.Drawable.product (A.render a) (B.render b)

  end
