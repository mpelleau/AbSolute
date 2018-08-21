(* Reduced product of domains A and B where B is more expressive than A *)

open Apron
open Adcp_sig
open ADCP

module type Reduction =
  sig
    module A : AbstractCP
    module B : AbstractCP
    val a_meet_b : A.t -> B.t -> B.t
    val b_meet_a : A.t -> B.t -> A.t
  end

module BoxAndPolyNew =
  struct

    module A=Abstract_box.BoxF
    module B=PolyCP

    (* converts box into a polyhedron and return its intersection with poly *)
    let to_poly box poly =
      let poly' = Abstractext.change_environment B.man B.empty (Abstractext.env poly) false in
      let (ivar, rvar) = B.T.apron_to_var poly in
      let vars = List.append ivar rvar in
      let l_expr = A.to_expr box vars in
      let exprs = B.T.apron_to_bexpr poly in
      let poly' = List.fold_left (fun a c -> B.filter a c) poly' (l_expr@exprs) in
      poly'

    let to_box poly box =
      let polycons = B.T.apron_to_bexpr poly in
      let box' = A.lfilter box polycons in
      box'

    let a_meet_b box poly =
      let poly' = to_poly box poly in
      Abstractext.meet B.man poly poly'

    let b_meet_a box poly =
      let box' = to_box poly box in
      A.meet box box'

  end

module BoxAndPoly =
  struct

    module A=BoxCP
    module B=PolyCP

    let a_meet_b box poly =
      let poly_env = Abstract1.env poly in
      let poly' = A.to_poly box poly_env in
      let poly' = Abstract1.meet B.man poly poly' in
      Format.printf "box = %a\npoly = %a\npoly' = %a@." A.print box B.print poly B.print poly';
      poly'

    let b_meet_a box poly =
      let box_env = Abstract1.env box in
      let box' = B.to_box poly box_env in
      let box' = Abstract1.meet BoxCP.man box box' in
      Format.printf "box = %a\npoly = %a\nbox' = %a@." A.print box B.print poly A.print box';
      box'

  end

module BoxAndOct =
  struct

    module A = BoxCP
    module B = OctBoxCP

    let a_meet_b box oct =
      let oct_env = Abstract1.env oct in
      let oct' = A.to_oct box oct_env in
      Abstract1.meet B.man oct oct'

    let b_meet_a box oct =
      let box_env = Abstract1.env box in
      let box' = OctBoxCP.to_box oct box_env in
      Abstract1.meet BoxCP.man box box'

  end

module OctAndPoly =
  struct

    module A = OctBoxCP
    module B = PolyCP

    let a_meet_b oct poly =
      let poly_env = Abstract1.env poly in
      let poly' = OctBoxCP.to_poly oct poly_env in
      Abstract1.meet PolyCP.man poly poly'

    let b_meet_a oct poly =
      let oct_env = Abstract1.env oct in
      let oct' = PolyCP.to_oct poly oct_env in
      Abstract1.meet OctBoxCP.man oct oct'

  end


module VariousDomain_MS (Reduced : Reduction) =
  struct

    include Reduced

    type t = A.t * B.t

    let reduced_product a b =
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
      let la,ua = A.prune a a'
      and lb,ub = B.prune b b' in
      let l = List.fold_left (fun acc ea ->
                  List.fold_left (fun lacc eb -> (ea, eb)::lacc) acc lb
                ) [] la in
      (List.filter (fun (abs, abs') -> not (is_empty (reduced_product abs abs')) ) l),(ua, ub)

    let split ((abs, abs'):t) =
      let split_a = A.split abs in
      List.map (fun x -> (x, abs')) split_a

    let is_enumerated (abs, abs') =
      A.is_enumerated abs && B.is_enumerated abs'

    let join (a,a') (b,b') = (A.join a b), (B.join a' b')

    let filter ((abs, abs'):t) cons =
      (A.filter abs cons, abs')

    let forward_eval (abs, abs') cons =
      let abs_tmp = a_meet_b abs abs' in
      B.forward_eval abs_tmp cons

    let print fmt ((abs, abs'):t) =
      A.print fmt abs;
      Format.printf ", ";
      B.print fmt abs'

    let volume ((abs, abs'):t) =
      B.volume (a_meet_b abs abs')

  end

module BandP = VariousDomain_MS(BoxAndPolyNew)
module BoxNOct = VariousDomain_MS(BoxAndOct)
module BoxNPoly = VariousDomain_MS(BoxAndPoly)
module OctNPoly = VariousDomain_MS(OctAndPoly)
