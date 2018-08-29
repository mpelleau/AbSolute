(* Reduced product of domains A and B where B is more expressive than A *)

open Adcp_sig

module type Reduction =
  sig
    module A : AbstractCP
    module B : AbstractCP
    type t = A.t * B.t

    val var_bounds : t -> Csp.var -> (Mpqf.t * Mpqf.t)

    val add_var : t -> Csp.annot * Csp.var -> t

    val filter : t -> (Csp.expr * Csp.cmpop * Csp.expr) -> t

    val reduced_product : A.t -> B.t -> (A.t * B.t)

    val print : Format.formatter -> t -> unit

    val is_empty : t -> bool

  end

module MakeProduct (A : AbstractCP) (B : AbstractCP)  =
  struct

    module A = A
    module B = B
    type t = A.t * B.t

    let is_representable = B.is_representable

    let to_bexpr (a, b) = (A.to_bexpr a)@(B.to_bexpr b)

    let a_meet_b a b =
      let b_expr = B.to_bexpr b in
      let b_vars = B.vars b in
      let a_expr = A.to_bexpr a in
      let a_vars = A.vars a in
      let to_add = List.fold_left (fun acc vb ->
                       if List.exists (fun va -> va = vb) a_vars then
                         acc
                       else
                         vb::acc
                     ) [] b_vars in
      let b' = List.fold_left (fun abs v -> B.add_var abs v) B.empty (a_vars@to_add) in
      List.fold_left (fun abs c -> B.filter abs c) b' (a_expr@b_expr)

    let b_meet_a a b =
      let b_expr = B.to_bexpr b in
      let b_vars = B.vars b in
      let a_expr = A.to_bexpr a in
      let a_vars = A.vars a in
      let to_add = List.fold_left (fun acc vb ->
                       if List.exists (fun va -> va = vb) a_vars then
                         acc
                       else
                         vb::acc
                     ) [] b_vars in
      let a' = List.fold_left (fun abs v -> A.add_var abs v) A.empty (a_vars@to_add) in
      List.fold_left (fun abs c -> A.filter abs c) a' (a_expr@b_expr)

    let reduced_product (a:A.t) (b:B.t) : (A.t * B.t) =
      let new_a = b_meet_a a b in
      let new_b = a_meet_b a b in
      (new_a, new_b)

    let empty = A.empty,B.empty

    let add_var (abs,abs') v = (A.add_var abs v), (B.add_var abs' v)

    let var_bounds (abs,abs') v  =
      let (la, ha) = A.var_bounds abs v
      and (lb, hb) = B.var_bounds abs' v in
      ((max la lb), (min ha hb))

    let rem_var (abs,abs') v =
       let a = A.rem_var abs v
       and b = B.rem_var abs' v in
       (a, b)

    let bound_vars (abs,abs')  =
      let la = A.bound_vars abs
      and lb = B.bound_vars abs' in
      let (tmp, _) = List.split lb in
      let (same, diffa) = List.partition (fun (v, c) -> List.mem v tmp) la in
      let (tmp, _) = List.split same in
      let (_, diffb) = List.partition (fun (v, c) -> List.mem v tmp) lb in
      List.append la diffb

    let vars (abs, abs') =
      let va = A.vars abs
      and vb = B.vars abs' in
      List.sort_uniq (compare) (va@vb)

    let is_small ((abs, abs'):t) = A.is_small abs

    let is_empty (abs, abs') = A.is_empty abs || B.is_empty abs'

    let prune (a, b) (a', b') =
      let la, ua =  A.prune a a'
      and lb, ub = if b = b' then ([b], b) else B.prune b b' in
      let l = List.fold_left (fun acc ea ->
                  List.fold_left (fun lacc eb -> (ea, eb)::lacc) acc lb
                ) [] la in
      let l' = List.filter (fun (abs, abs') ->
                   try (not (is_empty (reduced_product abs abs')))
                   with Bot.Bot_found -> false
                 ) l in
      l',(ua, ub)

    let split ((abs, abs'):t) =
      let split_a = A.split abs in
      List.map (fun x -> (x, abs')) split_a

    let join (a,a') (b,b') = (A.join a b), (B.join a' b')

    let filter ((abs, abs'):t) ((e1, op, e2) as cons) =
      match B.is_representable (Csp.Cmp(op, e1, e2)) with
      | Yes -> (A.filter abs cons, B.filter abs' cons)
      | Maybe | No -> (A.filter abs cons, abs')


    let forward_eval (abs, abs') cons =
      let abs_tmp = a_meet_b abs abs' in
      B.forward_eval abs_tmp cons

    let print fmt ((abs, abs'):t) =
      A.print fmt abs;
      Format.printf ", ";
      B.print fmt abs'

    let volume ((abs, abs'):t) =
      B.volume (a_meet_b abs abs')

    (* concretization function. we call it a spawner.
     useful to do tests, and to reuse the results.
     values are generated randomly *)
    let spawn (a,b) =
      let rec generate () =
        let i = A.spawn a in
        if B.is_abstraction b i then i
        else generate ()
      in generate ()

    let is_abstraction (a,b) i =
      A.is_abstraction a i && B.is_abstraction b i


  end

module BoxAndPolyNew = MakeProduct (Abstract_box.BoxF) (ADCP.PolyCP)
module BoxAndOct = MakeProduct (ADCP.BoxCP) (ADCP.OctBoxCP)
module BoxAndPoly = MakeProduct (ADCP.BoxCP) (ADCP.PolyCP)
module OctAndPoly = MakeProduct (ADCP.OctBoxCP) (ADCP.PolyCP)
