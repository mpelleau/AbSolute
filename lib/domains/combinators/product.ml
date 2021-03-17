(* Reduced product of domains A and B where B is more expressive than A *)

open Signature
open Consistency

module Make (A : Domain) (B : Domain) = struct
  type t = A.t * B.t

  type internal_constr = A.internal_constr * B.internal_constr

  let internalize ?elem (c : Constraint.t) =
    match elem with
    | None -> (A.internalize c, B.internalize c)
    | Some (a, b) -> (A.internalize ~elem:a c, B.internalize ~elem:b c)

  let externalize (c, _) = A.externalize c

  let is_representable (c1, c2) =
    Kleene.or_kleene (A.is_representable c1) (B.is_representable c2)

  let to_constraint (a, b) =
    Constraint.And (A.to_constraint a, B.to_constraint b)

  let a_meet_b a b : B.t Consistency.t =
    A.to_constraint a |> B.internalize |> B.filter b |> Consistency.map fst

  let b_meet_a a b : A.t Consistency.t =
    B.to_constraint b |> A.internalize |> A.filter a |> Consistency.map fst

  let reduced_product (a : A.t) (b : B.t) : (A.t * B.t) Consistency.t =
    let new_a = b_meet_a a b in
    match new_a with
    | Sat -> Consistency.map (fun b -> (a, b)) (a_meet_b a b)
    | Unsat -> Unsat
    | Filtered (a', success1) -> (
      match a_meet_b a' b with
      | Sat -> Filtered ((a', b), success1)
      | Unsat -> Unsat
      | Filtered (b', success2) -> Filtered ((a', b'), success1 && success2) )

  let empty = (A.empty, B.empty)

  let add_var (a, b) v = (A.add_var a v, B.add_var b v)

  let rm_var (a, b) v = (A.rm_var a v, B.rm_var b v)

  let vars (abs, abs') =
    let va = A.vars abs and vb = B.vars abs' in
    List.sort_uniq compare (va @ vb)

  let diff : (t -> t -> t list) option = None

  let split f ((a, b) : t) =
    let split_a = A.split f a in
    List.map (fun x -> (x, b)) split_a

  let join (a, b) (a', b') =
    let a, exact_a = A.join a a' in
    let b, exact_b = B.join b b' in
    ((a, b), exact_a && exact_b)

  let meet (a, b) (a', b') =
    try
      Tools.meet_bot
        (fun a b ->
          match reduced_product a b with
          | Sat -> (a, b)
          | Unsat -> raise Exit
          | Filtered (x, _) -> x )
        (A.meet a a') (B.meet b b')
    with Exit -> None

  let filter ((a, b) : t) (c1, c2) : (t * internal_constr) Consistency.t =
    let open Kleene in
    match B.is_representable c2 with
    | True -> Consistency.map (fun (b, c) -> ((a, b), (c1, c))) (B.filter b c2)
    | Unknown | False ->
        Consistency.map (fun (a, c) -> ((a, b), (c, c2))) (A.filter a c1)

  let forward_eval ((a, b) : t) obj =
    let l_a, u_a = A.forward_eval a obj in
    let l_b, u_b = B.forward_eval b obj in
    (max l_a l_b, min u_a u_b)

  let print fmt ((abs, abs') : t) =
    Format.fprintf fmt "%a, %a" A.print abs B.print abs'

  let volume ((_, abs') : t) = B.volume abs'

  (* concretization function *)
  let spawn (a, b) =
    let rec generate () =
      let i = A.spawn a in
      if B.is_abstraction b i then i else generate ()
    in
    generate ()

  let is_abstraction (a, b) i = A.is_abstraction a i && B.is_abstraction b i

  let render (a, b) = Picasso.Drawable.product (A.render a) (B.render b)
end
