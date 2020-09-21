open Apron
open Apronext
open Apron_utils

(* build the pair of constraints var >= value and var <= value *)
let complementary env var value =
  let e1 = Linexpr1.make env in
  Linexpr1.set_list e1 [Coeffext.minus_one, var] (Some (Coeff.Scalar value));
  let e2 =  Linexpr1.make env in
  Linexpr1.set_list e2 [Coeffext.one, var] (Some (Coeff.Scalar (Scalar.neg value)));
  e1,e2

(** Module for the Box Abstract Domains for Constraint Programming. *)
module BoxCP = struct
  include Apron_domain.MAKE(Box)

  let is_representable _ = Kleene.True

  let is_small box : bool =
    let (_, _, max) = largest box in
    let dim = Mpqf.to_float max in
    (dim <= !Constant.precision)

  let split abs _jacobian =
    let env = Abstract1.env abs in
    let (var, itv, _size) = largest abs in
    let mid = Intervalext.mid itv in
    let e1,e2 = complementary env var mid in
    split abs (e1,e2)

  let volume abs =
    let b = Abstract1.to_box man abs in
    b.Abstract1.interval_array
    |> Array.fold_left (fun v i -> Q.mul v (Intervalext.range_mpqf i)) Q.one
    |> Q.to_float

end

(** Module for the Octagon Abstract Domains for Constraint Programming. *)
module OctMinMinCP = struct

  include Apron_domain.MAKE (Oct)

  (* Compute the max and the min for the basis Bij *)
  let minmax_bij oct i j =
    let env = Abstract1.env oct in
    let open Coeffext in
    let ci = one,i and cj = one,j in
    let minus_ci = minus_one,i and minus_cj = minus_one,j in
    let e1 = Linexpr1.make env in
    Linexpr1.set_list e1 [ci ; cj] None;
    let e2 = Linexpr1.make env in
    Linexpr1.set_list e2 [ci ; minus_cj] None;
    let e1' = Linexpr1.make env in
    Linexpr1.set_list e1' [minus_ci; minus_cj] None;
    let e2' = Linexpr1.make env in
    Linexpr1.set_list e2' [minus_ci ; cj] None;
    (* Get the interval for the expression "vi + vj" and "vi -vj" *)
    let itv1 = Abstract1.bound_linexpr man oct e1 in
    let diam1 = Intervalext.range itv1 in
    let itv2 = Abstract1.bound_linexpr man oct e2 in
    let diam2 = Intervalext.range itv2 in
    if Scalar.cmp diam1 diam2 > 0 then
      e1,e1',itv1,(scalar_mul_sqrt2 diam1),(scalar_mul_sqrt2 diam2)
    else
      e2,e2',itv2,(scalar_mul_sqrt2 diam2),(scalar_mul_sqrt2 diam1)

  let is_small octad =
    (*printf "oct = %a@." Abstract1.print octad;*)
    let env = Abstract1.env octad in
    let dim = Environment.size env in
    (* For a given i, compute the min of all the min for the basis Bij
     * and the corresopnding max.
     * min_{j \in \{i+1...n\}} (min Bij) *)
    let rec minmax_bi var_i j min max cst linexpr1 linexpr2 =
      if j >= dim then (linexpr1, linexpr2, cst, max, min)
      else
	      let var_j = Environment.var_of_dim env j in
        let (expr_max, expr_max', itv_max, diam_max, diam_min) = minmax_bij octad var_i var_j in
        if Scalar.cmp diam_min min < 0 then
          minmax_bi var_i (j+1) diam_min diam_max (Intervalext.mid itv_max) expr_max expr_max'
        else
          minmax_bi var_i (j+1) min max cst linexpr1 linexpr2
    in

    (* Compute the min of all the min of the basis Bij and the corresponding max
     * min_{i \in \{1...n\}, j \in \{i+1...n\}} (min Bij) *)
    let rec minmax_b i min max cst linexpr1 linexpr2 =
      if i >= dim-1 then max
      else
        let var_i = Environment.var_of_dim env i in
        let (linexpr1', linexpr2', cst', max', min') = minmax_bi var_i (i+1) min max cst linexpr1 linexpr2 in
        if Scalar.cmp min' min < 0 then minmax_b (i+1) min' max' cst' linexpr1' linexpr2'
        else minmax_b (i+1) min max cst linexpr1 linexpr2
    in

    let box = Abstract1.to_box man octad in
    let tab = box.Abstract1.interval_array in
    let (mmax, v, mmin) = minmax tab 1 (Intervalext.range_mpqf tab.(0)) 0 (Intervalext.range_mpqf tab.(0)) in
    let mmax' = Scalar.of_mpqf mmax in
    let expr1 = Linexpr1.make env in
    Linexpr1.set_list expr1 [(Coeffext.one, Environment.var_of_dim env v)] None;
    let expr2 = Linexpr1.make env in
    Linexpr1.set_list expr2 [(Coeffext.minus_one, Environment.var_of_dim env v)] None;
    let mmin' = Scalar.of_mpqf mmin in
    let max = minmax_b 0 mmin' mmax' (Intervalext.mid tab.(v)) expr1 expr2 in
    let max = Scalarext.to_float max in
    (max <= !Constant.precision)

  let split octad =
    let env = Abstract1.env octad in
    let dim = Environment.size env in
    (* For a given i, compute the min of all the min for the basis Bij
     * and the corresopnding max.
     * min_{j \in \{i+1...n\}} (min Bij) *)
    let rec minmax_bi var_i j min max cst linexpr1 linexpr2 =
      if j >= dim then (linexpr1, linexpr2, cst, max, min)
      else
	      let var_j = Environment.var_of_dim env j in
        let (expr_max, expr_max', itv_max, diam_max, diam_min) = minmax_bij octad var_i var_j in
        if Scalar.cmp diam_min min < 0 then
          minmax_bi var_i (j+1) diam_min diam_max (Intervalext.mid itv_max) expr_max expr_max'
        else
          minmax_bi var_i (j+1) min max cst linexpr1 linexpr2
    in
    let rec minmax_b i min max cst linexpr1 linexpr2 =
      if i >= dim-1 then (linexpr1, linexpr2, cst, max, min)
      else
        let var_i = Environment.var_of_dim env i in
        let (linexpr1', linexpr2', cst', max', min') = minmax_bi var_i (i+1) min max cst linexpr1 linexpr2 in
        if Scalar.cmp min' min < 0 then minmax_b (i+1) min' max' cst' linexpr1' linexpr2'
        else minmax_b (i+1) min max cst linexpr1 linexpr2
    in

    let box = Abstract1.to_box man octad in
    let tab = box.Abstract1.interval_array in
    let (mmax, i_max, mmin) = minmax tab 1 (Intervalext.range_mpqf tab.(0)) 0 (Intervalext.range_mpqf tab.(0)) in
    let mmax' = Scalar.of_mpqf mmax in
    let expr1 = Linexpr1.make env in
    Linexpr1.set_list expr1 [(Coeffext.one, Environment.var_of_dim env i_max)] None;
    let expr2 = Linexpr1.make env in
    Linexpr1.set_list expr2 [(Coeffext.minus_one, Environment.var_of_dim env i_max)] None;
    let mmin' = Scalar.of_mpqf mmin in
    let (linexpr1, linexpr2, _cst, _max, _min) = minmax_b 0 mmin' mmax' (Intervalext.mid tab.(i_max)) expr1 expr2 in
    split octad (linexpr1,linexpr2)

  let volume _ = 0.
end

(** Module for the Octagon Abstract Domains for Constraint Programming. *)
module OctMinMaxCP = struct

  include Apron_domain.MAKE (Oct)

  let is_small octad =
    (*printf "oct = %a@." Abstract1.print octad;*)
    let env = Abstract1.env octad in
    let dim = Environment.size env in

    (* Compute the max for the basis Bij *)
    let max_bij var_i var_j =
      let expr1 = Linexpr1.make env in
      Linexpr1.set_list expr1 [(Coeffext.one, var_i) ; (Coeffext.one, var_j)] None;
      let expr2 = Linexpr1.make env in
      Linexpr1.set_list expr2 [(Coeffext.one, var_i) ; (Coeffext.minus_one, var_j)] None;

      let expr1' = Linexpr1.make env in
      Linexpr1.set_list expr1' [(Coeffext.minus_one, var_i) ; (Coeffext.minus_one, var_j)] None;
      let expr2' = Linexpr1.make env in
      Linexpr1.set_list expr2' [(Coeffext.minus_one, var_i) ; (Coeffext.one, var_j)] None;

      (* Get the interval for the expression "vi + vj" and "vi -vj" *)
      let itv1 = Abstract1.bound_linexpr man octad expr1 in
      let diam1 = Intervalext.range itv1 in
      let itv2 = Abstract1.bound_linexpr man octad expr2 in
      let diam2 = Intervalext.range itv2 in
      let cond = (Scalar.cmp diam1 diam2 > 0) in
      let diam_tmp = if cond then diam1 else diam2 in
      (*printf "%a ? %a : %a@." Scalar.print diam1 Scalar.print diam2 Scalar.print diam_tmp;*)
      let expr_tmp = if cond then expr1 else expr2 in
      let expr_tmp' = if cond then expr1' else expr2' in
      let itv_tmp = if cond then itv1 else itv2 in
      (expr_tmp, expr_tmp', itv_tmp, diam_tmp)
    in

    (* For a given i, compute the min of all the max for the basis Bij
     * min_{j \in \{i+1...n\}} (max Bij) *)
    let rec max_bi var_i j min_max cst linexpr1 linexpr2 =
      if j >= dim then
        (linexpr1, linexpr2, cst, min_max)
      else
        let var_j = Environment.var_of_dim env j in
        let (expr_tmp1, expr_tmp2, itv_tmp, diam_tmp) = max_bij var_i var_j in
        if Scalar.cmp diam_tmp min_max < 0 then
          max_bi var_i (j+1) diam_tmp (Intervalext.mid itv_tmp) expr_tmp1 expr_tmp2
        else
          max_bi var_i (j+1) min_max cst linexpr1 linexpr2
    in

    (* Compute the min of all the max of the basis Bij
     * min_{i \in \{1...n\}, j \in \{i+1...n\}} (max Bij) *)
    let rec max i min_max cst linexpr1 linexpr2 =
      if i >= dim-1 then
        (linexpr1, linexpr2, cst, min_max)
      else
        let var_i = Environment.var_of_dim env i in
        let (linexpr1', linexpr2', cst', min_max') = max_bi var_i (i+1) min_max cst linexpr1 linexpr2 in
        if Scalar.cmp min_max' min_max < 0 then
          max (i+1) min_max' cst' linexpr1' linexpr2'
        else
          max (i+1) min_max cst linexpr1 linexpr2
    in
    let (var, itv, mmax) = largest octad in
    let mmax' = Scalar.of_mpqf mmax in
    let expr1 = Linexpr1.make env in
    Linexpr1.set_list expr1 [(Coeffext.one, var)] None;
    let expr2 = Linexpr1.make env in
    Linexpr1.set_list expr2 [(Coeffext.minus_one, var)] None;
    let (linexpr1, linexpr2, cst, min_max) = max 0 mmax' (Intervalext.mid itv) expr1 expr2 in
    Linexpr1.set_cst linexpr1 (Coeff.Scalar (Scalar.neg cst));
    Linexpr1.set_cst linexpr2 (Coeff.Scalar cst);

    let max = Scalarext.to_float min_max in
    (*printf "max = %f@.split = [%a, %a]@." max Linexpr1.print linexpr1 Linexpr1.print linexpr2;*)
    (max <= !Constant.precision)

  let split octad _ =
    let env = Abstract1.env octad in
    let poly = to_poly octad env in
    split octad (get_expr (Polka.manager_alloc_strict()) poly)

  let volume _ = 0.
end

(** Module for the Octagon Abstract Domains for Constraint Programming. *)
module OctBoxCP = struct

  include Apron_domain.MAKE (Oct)

  let is_representable c = Csp_helper.is_cons_linear c |> Kleene.of_bool

  let is_small oct =
    let (_, _,max) = largest oct in
    let dim = Mpqf.to_float max in
    (dim <= !Constant.precision)

  let split octad _ =
    let env = Abstract1.env octad in
    let (var, itv, _) = largest octad in
    let mid = Intervalext.mid itv in
    let e1,e2 = complementary env var mid in
    split octad (e1, e2)

  let volume _ = 0.
end

(** Module for the Polyhedron Abstract Domains for Constraint Programming. *)
module PolyCP = struct
  include Apron_domain.MAKE (struct
              type t = Polka.strict Polka.t
              let manager_alloc = Polka.manager_alloc_strict
            end)

  let is_small poly = is_small man poly

  let rec is_representable c =
    match c with
    | Csp.Cmp(_, _, _) -> Csp_helper.is_cons_linear c |> Kleene.of_bool
    | Csp.And(e1, e2) -> Kleene.and_kleene (is_representable e1) (is_representable e2)
    | Csp.Not(e) -> Kleene.not_kleene (is_representable e)
    | _ -> Kleene.False

  let split poly _ =
    split poly (get_expr (Polka.manager_alloc_strict()) poly)

  let prune a b =
    let work acc a c =
      let neg_c = Linconsext.neg c in
      let a' = A.filter_lincons man a c
      and s = A.filter_lincons man a neg_c in
      if is_empty s then a,acc
      else a',(s::acc)
    in
    let _,pruned = Linconsext.array_fold
                     (fun (_,acc) c ->
                       if Linconsext.get_typ c = Lincons1.EQ then
                         let c1,c2 = Linconsext.spliteq c in
                         let a',acc' = work acc a c1 in
                         work acc' a' c2
                       else work acc a c
                     ) (a,[]) (A.to_lincons_array man b)
    in pruned

  let prune = Some prune

  let volume _ = 0.

end
