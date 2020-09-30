(* Reduced product of domains A and B where B is more expressive than A *)

open Signature
open Consistency

module Make (A:Domain) (B:Domain) = struct

  type t = A.t * B.t

  let is_representable = B.is_representable

  let to_bexpr (a, b) = Csp.And (A.to_bexpr a, B.to_bexpr b)

  let a_meet_b a b : B.t Consistency.t =
    A.to_bexpr a |> B.filter b

  let b_meet_a a b : A.t Consistency.t =
     B.to_bexpr b |>  A.filter a

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

  let add_var (a,b) v = (A.add_var a v), (B.add_var b v)

  let var_bounds (a,b) v =
    Bot.debot (Itv.ItvQ.meet (A.var_bounds a v) (B.var_bounds b v))

  let rm_var (a,b) v = (A.rm_var a v),(B.rm_var b v)

  let bounds (a,b) =
    List.fold_left (fun acc (v,i) ->
        (v, Bot.debot (Itv.ItvQ.meet i (B.var_bounds b v)))::acc
      ) [] (A.bounds a)

  let vars (abs, abs') =
    let va = A.vars abs
    and vb = B.vars abs' in
    List.sort_uniq (compare) (va@vb)

  let is_empty (abs, abs') = A.is_empty abs || B.is_empty abs'

  let prune : (t -> t -> t list) option = None

  let split ((abs, abs'):t) (jacobian : Csp.ctrs) =
    let split_a = A.split abs jacobian in
    List.map (fun x -> (x, abs')) split_a

  let join (a,b) (a',b') =
    let a,exact_a = A.join a a' in
    let b,exact_b = B.join b b' in
    (a,b),(exact_a && exact_b)

  let meet (a,a') (b,b') =
    match reduced_product (A.meet a b) (B.meet a' b') with
    | Sat -> (a,a')
    | Unsat -> raise Bot.Bot_found
    | Filtered (x,_) -> x

  let filter ((a, b):t) c : t Consistency.t =
    let open Kleene in
    match B.is_representable c with
    | True -> Consistency.map (fun b -> a,b) (B.filter b c)
    | Unknown | False -> Consistency.map (fun a -> a,b) (A.filter a c)

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
