open Apron
open Apronext
open Apron_domain

(* build the pair of constraints var >= value and var <= value *)
let complementary env var value =
  let value = Coeff.Scalar value in
  let e1 = Linexpr1.make env in
  Linexpr1.set_list e1 [(Coeffext.minus_one, var)] (Some value) ;
  let e2 = Linexpr1.make env in
  Linexpr1.set_list e2 [(Coeffext.one, var)] (Some (Coeff.neg value)) ;
  (e1, e2)

(** Module for the Box Abstract Domains for Constraint Programming. *)
module BoxCP = struct
  include Apron_domain.MAKE (Box)

  let is_representable _ = Kleene.True

  let split prec abs =
    let env = Abstract1.env abs in
    let var, itv, size = largest abs in
    if Mpqf.to_float size < prec then raise Signature.TooSmall
    else
      let mid = Intervalext.mid itv in
      let e1, e2 = complementary env var mid in
      split abs (e1, e2)
end

(** Module for the Octagon Abstract Domains for Constraint Programming. *)
module OctMinMinCP = struct
  include Apron_domain.MAKE (Oct)

  (* Compute the max and the min for the basis Bij *)
  let minmax_bij oct i j =
    let env = Abstract1.env oct in
    let open Coeffext in
    let ci = (one, i) and cj = (one, j) in
    let minus_ci = (minus_one, i) and minus_cj = (minus_one, j) in
    let e1 = Linexpr1.make env in
    Linexpr1.set_list e1 [ci; cj] None ;
    let e2 = Linexpr1.make env in
    Linexpr1.set_list e2 [ci; minus_cj] None ;
    let e1' = Linexpr1.make env in
    Linexpr1.set_list e1' [minus_ci; minus_cj] None ;
    let e2' = Linexpr1.make env in
    Linexpr1.set_list e2' [minus_ci; cj] None ;
    (* Get the interval for the expression "vi + vj" and "vi -vj" *)
    let itv1 = Abstract1.bound_linexpr man oct e1 in
    let diam1 = Intervalext.range itv1 in
    let itv2 = Abstract1.bound_linexpr man oct e2 in
    let diam2 = Intervalext.range itv2 in
    if Scalar.cmp diam1 diam2 > 0 then
      (e1, e1', itv1, scalar_mul_sqrt2 diam1, scalar_mul_sqrt2 diam2)
    else (e2, e2', itv2, scalar_mul_sqrt2 diam2, scalar_mul_sqrt2 diam1)

  let split octad =
    let env = Abstract1.env octad in
    let dim = E.size env in
    (* For a given i, compute the min of all the min for the basis Bij
     * and the corresopnding max.
     * min_{j \in \{i+1...n\}} (min Bij) *)
    let rec minmax_bi var_i j min max cst linexpr1 linexpr2 =
      if j >= dim then (linexpr1, linexpr2, cst, max, min)
      else
        let var_j = E.var_of_dim env j in
        let expr_max, expr_max', itv_max, diam_max, diam_min =
          minmax_bij octad var_i var_j
        in
        if Scalar.cmp diam_min min < 0 then
          minmax_bi var_i (j + 1) diam_min diam_max (Intervalext.mid itv_max)
            expr_max expr_max'
        else minmax_bi var_i (j + 1) min max cst linexpr1 linexpr2
    in
    let rec minmax_b i min max cst linexpr1 linexpr2 =
      if i >= dim - 1 then (linexpr1, linexpr2)
      else
        let var_i = E.var_of_dim env i in
        let linexpr1', linexpr2', cst', max', min' =
          minmax_bi var_i (i + 1) min max cst linexpr1 linexpr2
        in
        if Scalar.cmp min' min < 0 then
          minmax_b (i + 1) min' max' cst' linexpr1' linexpr2'
        else minmax_b (i + 1) min max cst linexpr1 linexpr2
    in
    let box = Abstract1.to_box man octad in
    let tab = box.Abstract1.interval_array in
    let range = Intervalext.range_mpqf tab.(0) in
    let mmax, i_max, mmin = minmax tab 1 range 0 range in
    let e1 = Linexpr1.make env in
    Linexpr1.set_list e1 [(Coeffext.one, E.var_of_dim env i_max)] None ;
    let e2 = Linexpr1.make env in
    Linexpr1.set_list e2 [(Coeffext.minus_one, E.var_of_dim env i_max)] None ;
    let l1, l2 = minmax_b 0 mmin mmax (Intervalext.mid tab.(i_max)) e1 e2 in
    split octad (l1, l2)
end

(** Module for the Octagon Abstract Domains for Constraint Programming. *)
module OctMinMaxCP = struct
  include Apron_domain.MAKE (Oct)

  let split prec octad =
    let env = Abstract1.env octad in
    let poly = to_poly octad env in
    split octad (get_expr prec poly)
end

(** Module for the Octagon Abstract Domains for Constraint Programming. *)
module OctCP = struct
  include Apron_domain.MAKE (Oct)

  let is_representable _ = Kleene.True

  let split prec octad =
    let env = Abstract1.env octad in
    let var, itv, size = largest octad in
    if Mpqf.to_float size < prec then raise Signature.TooSmall
    else
      let mid = Intervalext.mid itv in
      let e1, e2 = complementary env var mid in
      split octad (e1, e2)
end

(** Module for the Polyhedron Abstract Domains for Constraint Programming. *)
module PolyCP = struct
  include Apron_domain.MAKE (struct
    type t = Polka.strict Polka.t

    let manager_alloc = Polka.manager_alloc_strict
  end)

  let is_representable _ = Kleene.True

  let split prec poly = split poly (get_expr prec poly)

  let diff =
    let neg acc a c =
      let neg_c = Linconsext.neg c in
      let a' = A.filter_lincons man a c and s = A.filter_lincons man a neg_c in
      if is_empty s then (a, acc) else (a', s :: acc)
    in
    Some
      (fun a b ->
        let _, pruned =
          Linconsext.array_fold
            (fun (_, acc) c ->
              if Linconsext.get_typ c = Lincons1.EQ then
                let c1, c2 = Linconsext.spliteq c in
                let a', acc' = neg acc a c1 in
                neg acc' a' c2
              else neg acc a c)
            (a, []) (A.to_lincons_array man b)
        in
        pruned)
end
