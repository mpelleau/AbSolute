
(** 
 * This is an implementation of a CP solver using APRON.
 *)

open Apron;;
open Mpqf;;
open Format;;

let split_prec = 0.001;;
let split_prec_mpqf = Mpqf.of_float split_prec;;

let print_array = Abstract0.print_array;;
let lincons1_array_print fmt x =
  Lincons1.array_print fmt x
;;
let generator1_array_print fmt x =
  Generator1.array_print fmt x
;;


(******************************************************************)
(***************** Different conversion operators *****************)
(******************************************************************)

let scalar_to_mpqf = function
  | Scalar.Mpqf x -> x
  | Scalar.Float x -> Mpqf.of_float x
  | Scalar.Mpfrf x -> Mpfrf.to_mpqf x
;;

let scalar_to_float = function
  | Scalar.Mpqf x -> Mpqf.to_float x
  | Scalar.Float x -> x
  | Scalar.Mpfrf x -> Mpfrf.to_float ~round:Mpfr.Near x
;;

let coeff_to_float = function
  | Coeff.Scalar x -> scalar_to_float x
  | Coeff.Interval i -> scalar_to_float i.Interval.inf
;;

(** Converts a linear expression into its negation
 * ex: converts 3x-y into -3x+y
 *)
let linexpr_neg linexpr env =
  let linexpr' = Linexpr1.make env in
  let list = ref [] in
  let cst = Linexpr1.get_cst linexpr in
  Linexpr1.iter (fun c -> fun v -> list := List.append !list [(Coeff.neg c, v)]) linexpr;
  Linexpr1.set_list linexpr' !list (Some (Coeff.neg cst));
  linexpr'
;;

(** Converts a Generator0 into an array of floats. *)
let to_float_array gen size =
  let tab = Array.make size 0. in
  let gen_lin = gen.Generator0.linexpr0 in
  for i=0 to (size-1) do
    let coeff = Linexpr0.get_coeff gen_lin i in
    tab.(i) <- coeff_to_float coeff
  done;
  tab
;;

(** Converts a Generator1 into an array of array of floats. *)
let gen_to_array gens size =
  let gen_tab = gens.Generator1.generator0_array in
  let tab = Array.make (Array.length gen_tab) (Array.make size 0.) in
  for i=0 to ((Array.length gen_tab)-1) do
    tab.(i) <- to_float_array gen_tab.(i) size
  done;
  tab
;;

(** Converts a polyhedron into an octagon. *)
let poly_to_oct manpoly poly manoct oct_env =
  let poly' = Abstract1.change_environment manpoly poly oct_env false in
  let gens = Abstract1.to_generator_array manpoly poly' in
  let dim = Environment.dimension oct_env in
  let oct0 = Oct.of_generator_array manoct dim.Dim.intd dim.Dim.reald gens.Generator1.generator0_array in
  let oct = {Abstract1.abstract0 = oct0; Abstract1.env = oct_env} in
  oct
;;

(** Converts an octagon into a polyhedron. *)
let oct_to_poly manoct oct manpoly poly_env =
  let oct' = Abstract1.change_environment manoct oct poly_env false in
  let cons = Abstract1.to_lincons_array manoct oct' in
  let poly = Abstract1.of_lincons_array manpoly poly_env cons in
  poly
;;

(** Converts a polyhedron into a box. *)
let poly_to_box manpoly poly manbox box_env =
  let poly' = Abstract1.change_environment manpoly poly box_env false in
  let bounds = Abstract1.to_box manpoly poly' in
  let (ivars, rvars) = Environment.vars box_env in
  let tvars = Array.append ivars rvars in
  let box = Abstract1.of_box manbox box_env tvars bounds.Abstract1.interval_array in
  box
;;

(** Converts a box into a polyhedron. *)
let box_to_poly manbox box manpoly poly_env =
  let box' = Abstract1.change_environment manbox box poly_env false in
  let cons = Abstract1.to_lincons_array manbox box' in
  let poly = Abstract1.of_lincons_array manpoly poly_env cons in
  poly
;;

(** Converts an octagon into a box. *)
let oct_to_box manoct oct manbox box_env =
  let oct' = Abstract1.change_environment manoct oct box_env false in
  let bounds = Abstract1.to_box manoct oct' in
  let (ivars, rvars) = Environment.vars box_env in
  let tvars = Array.append ivars rvars in
  let box = Abstract1.of_box manbox box_env tvars bounds.Abstract1.interval_array in
  box
;;

(** Converts a box into an octagon. *)
let box_to_oct manbox box manoct oct_env =
  let box' = Abstract1.change_environment manbox box oct_env false in
  let bounds = Abstract1.to_box manbox box' in
  let (ivars, rvars) = Environment.vars oct_env in
  let tvars = Array.append ivars rvars in
  let oct = Abstract1.of_box manoct oct_env tvars bounds.Abstract1.interval_array in
  oct
;;


(******************************************************************)
(*********************** Various operators ************************)
(******************************************************************)

(* Compute the sum of two scalars *)
let scalar_add sca sca' = 
  let value = scalar_to_mpqf sca in
  let value' = scalar_to_mpqf sca' in
  let sum = Mpqf.add value value' in
  Scalar.of_mpqf sum
;;

(** Compute the medium value of two scalars *)
let mid inf sup = 
  let mpqf_inf = scalar_to_mpqf inf in
  let mpqf_sup = scalar_to_mpqf sup in
  Scalar.of_mpqf (Mpqf.add (Mpqf.div (Mpqf.add mpqf_inf mpqf_sup) (Mpqf.of_int 2)) split_prec_mpqf)
;;

(** Compute the middle value of an interval *)
let mid_interval itv =
  mid itv.Interval.inf itv.Interval.sup
;;

(** Compute the euclidian distance between two scalars *)
let diam inf sup = 
  let mpqf_inf = scalar_to_mpqf inf in
  let mpqf_sup = scalar_to_mpqf sup in
  Scalar.of_mpqf (Mpqf.sub mpqf_sup mpqf_inf)
;;

(** Compute the diameter od an interval *)
let diam_interval itv = 
  diam itv.Interval.inf itv.Interval.sup
;;

(** Compute the euclidian distance between two arrays of floats. *)
let dist tab1 tab2 =
  if Array.length tab1 != Array.length tab2 then
    failwith ("The two arrays must have the same length.")
  else
    let sum = ref 0. in
    for i=0 to ((Array.length tab1)-1) do
      sum := !sum +. ((tab1.(i) -. tab2.(i)) ** 2.)
    done;
    sqrt !sum
;;

(** Tests if the box is small enough (wrt prec). *)
let is_small box prec =
  
  let rec small tab i =
    if i>=Array.length tab then
      true
    else
      let mpqf_inf = scalar_to_mpqf (tab.(i)).Interval.inf in
      let mpqf_sup = scalar_to_mpqf (tab.(i)).Interval.sup in
      (Mpqf.cmp (Mpqf.sub mpqf_sup mpqf_inf) prec) <= 0 && small tab (i+1)
    ;
  in
  
  let itv = box.Abstract1.interval_array in
  small itv 0
;;

(** Tests if two arrays of Interval are equals. *)
let equals tab1 tab2 =

  let rec tab_eq tab1 tab2 i =
    if i>=Array.length tab1 then
      true
    else
      (Interval.equal tab1.(i) tab2.(i)) && (tab_eq tab1 tab2 (i+1));
  in
  
  if Array.length tab1 != Array.length tab2 then
    false
  else
    tab_eq tab1 tab2 0
;;

(**
 * Tests if an array of Interval is feasible.
 * Returns true if all its intervals are different of bottom, false otherwise.
 *)
let is_feasible tab =

  let rec is_bot tab i =
    if i>= Array.length tab then
      true
    else
      not (Interval.is_bottom tab.(i)) || (is_bot tab (i+1));
  in
  
  is_bot tab 0
;;

(**
 * Tests if an array of Interval is equal to top.
 * Returns true if all its intervals are equal to top, false otherwise.
 *)
let is_top tab =

  let rec top tab i =
    if i>= Array.length tab then
      true
    else
      (Interval.is_top tab.(i)) && (top tab (i+1));
  in
  
  top tab 0
;;

(** Create a new array of constraints depending on the differences between the two boxes. *)
let from_diff env box box' =
  let itv = box.Abstract1.interval_array in
  let itv' = box'.Abstract1.interval_array in
  
  (* Compute the number of differences *)
  let rec nb_diff i =
    if i >= (Array.length itv) then
      0
    else
      if Interval.equal itv.(i) itv'.(i) then
        nb_diff (i+1)
      else
        if not (Scalar.equal (itv.(i)).Interval.inf (itv'.(i)).Interval.inf) &&
           not (Scalar.equal (itv.(i)).Interval.sup (itv'.(i)).Interval.sup) &&
           not (Scalar.equal (itv'.(i)).Interval.inf (itv'.(i)).Interval.sup) then
           (* Both bounds have been changed to different values *)
          2 + (nb_diff (i+1))
        else
          1 + (nb_diff (i+1))
  in
  let nb = nb_diff 0 in
  let tab' = Lincons1.array_make env (nb) in
  
  (* Add the constraint "coeff*var + value typ 0" with typ in {>=, =} at the
   * line index of the table of linear constraints tab' *)
  let add_ctr coeff var typ value index =
    let cons = Lincons1.make (Linexpr1.make env) typ in
      Lincons1.set_array cons
      [|
        ((Coeff.s_of_int (coeff)), (Environment.var_of_dim env var))
      |]
      (Some (Coeff.Scalar value))
    ;
    Lincons1.array_set tab' index cons
  in
  
  let next = ref 0 in
  
  (* Add the constraints depending on the differences *)
  for i=0 to ((Array.length itv)-1) do
    let already_added = ref false in
    if not (Scalar.equal (itv.(i)).Interval.inf (itv'.(i)).Interval.inf) then
      if Scalar.equal (itv'.(i)).Interval.inf (itv'.(i)).Interval.sup then
        (* The variable var is assigned to a value val -> add the constraint var=val *)
        (
        add_ctr (-1) i Lincons1.EQ (itv'.(i)).Interval.inf !next;
        next := !next+1;
        already_added := true;
        )
      else
        (
        add_ctr 1 i Lincons1.SUPEQ (Scalar.neg (itv'.(i)).Interval.inf) !next;
        next := !next+1;
        )
    ;
    if not (Scalar.equal (itv.(i)).Interval.sup (itv'.(i)).Interval.sup) then
      if (Scalar.equal (itv'.(i)).Interval.inf (itv'.(i)).Interval.sup) then
        (
        if not (!already_added) then
          (* The variable var is assigned to a value val -> add the constraint var=val *)
          (
          add_ctr (-1) i Lincons1.EQ (itv'.(i)).Interval.inf !next;
          next := !next+1;
          )
        ;
        )
      else
        (
        add_ctr (-1) i Lincons1.SUPEQ (itv'.(i)).Interval.sup !next;
        next := !next+1;
        )
    ;
  done;
  
  tab'
;;

(**
 * Add the new constraint (linexpr typ 0) at the begining of tab.
 * typ can either be negative for <= or positive for >=
 *)
let copy linexpr typ env tab =
  let tab' = Lincons1.array_make env ((Lincons1.array_length tab)+1) in
  if typ < 0 then
    let linexpr' = linexpr_neg linexpr env in
    let cons = Lincons1.make linexpr' Lincons1.SUPEQ in
    Lincons1.array_set tab' 0 cons
  else
    let cons = Lincons1.make linexpr Lincons1.SUPEQ in
    Lincons1.array_set tab' 0 cons;
  ;
  
  for i=0 to ((Lincons1.array_length tab)-1) do
    Lincons1.array_set tab' (i+1) (Lincons1.array_get tab i)
  done;
  
  tab'
;;

(** Print the solution. *)
let print_sol box =
  let itv = box.Abstract1.interval_array in
  printf "solution = [|[%f; %f]" (scalar_to_float (itv.(0)).Interval.inf) (scalar_to_float (itv.(0)).Interval.sup);
  for i=1 to ((Array.length itv)-1) do
    printf "; [%f; %f]" (scalar_to_float (itv.(i)).Interval.inf) (scalar_to_float (itv.(i)).Interval.sup);
  done;
  printf "|]@.";
;;

(** Print the generators. *)
let print_linexpr0 expr env =
  Linexpr0.iter (fun c -> fun d -> printf "%f%s " (coeff_to_float c) (Var.to_string (Environment.var_of_dim env d))) expr;
;;

(** Print the generators. *)
let print_gen gens env =
  let gen_tab = gens.Generator1.generator0_array in
  printf "@.generators = {";
  print_linexpr0 (gen_tab.(0)).Generator0.linexpr0 env;
  for i=1 to ((Array.length gen_tab)-1) do
    printf "; ";
    print_linexpr0 (gen_tab.(i)).Generator0.linexpr0 env;
  done;
  printf "}@.";
;;


(******************************************************************)
(************************ Reduced Product *************************)
(******************************************************************)

let box_meet_oct manbox box manoct oct =
  let box_env = Abstract1.env box in
  let oct_env = Abstract1.env oct in
  let box2oct = box_to_oct manbox box manoct oct_env in
  Abstract1.meet_with manoct oct box2oct;
  let oct2box = oct_to_box manoct oct manbox box_env in
  Abstract1.meet_with manbox box oct2box;
;;

let box_meet_poly manbox box manpoly poly =
  let box_env = Abstract1.env box in
  let poly_env = Abstract1.env poly in
  let box2poly = box_to_poly manbox box manpoly poly_env in
  Abstract1.meet_with manpoly poly box2poly;
  let poly2box = poly_to_box manpoly poly manbox box_env in
  Abstract1.meet_with manbox box poly2box;
;;

let oct_meet_poly manoct oct manpoly poly =
  let oct_env = Abstract1.env oct in
  let poly_env = Abstract1.env poly in
  let oct2poly = oct_to_poly manoct oct manpoly poly_env in
  Abstract1.meet_with manpoly poly oct2poly;
  let poly2oct = poly_to_oct manpoly poly manoct oct_env in
  Abstract1.meet_with manoct oct poly2oct;
;;

let abs_meet_abs man abs abs' =
  let env = Abstract1.env abs in
  let env' = Abstract1.env abs' in
  let abs_tmp1 = Abstract1.change_environment man abs env' false in
  Abstract1.meet_with man abs' abs_tmp1;
  let abs_tmp2 = Abstract1.change_environment man abs' env false in
  Abstract1.meet_with man abs abs_tmp2;
;;

(**
 * Compute the reduced product of the two abstractions.
 *)
let reduced_product abs abs' =
  let man = Abstract1.manager abs in
  let man' = Abstract1.manager abs' in
  
  if (Box.manager_is_box man) && (Box.manager_is_box man') then
    (
    printf "box & box@.";
    abs_meet_abs man abs abs'
    )
  ;
  if (Box.manager_is_box man) && (Oct.manager_is_oct man') then
    (
    printf "box & oct@.";
    box_meet_oct man abs man' abs'
    )
  ;
  if (Box.manager_is_box man) && (Polka.manager_is_polka_strict man') then
    (
    printf "box & poly@.";
    box_meet_poly man abs man' abs'
    )
  ;
  
  if (Oct.manager_is_oct man) && (Box.manager_is_box man') then
    (
    printf "oct & box@.";
    box_meet_oct man' abs' man abs
    )
  ;
  if (Oct.manager_is_oct man) && (Oct.manager_is_oct man') then
    (
    printf "oct & oct@.";
    abs_meet_abs man abs abs'
    )
  ;
  if (Oct.manager_is_oct man) && (Polka.manager_is_polka_strict man') then
    (
    printf "oct & poly@.";
    oct_meet_poly man abs man' abs'
    )
  ;
  
  if (Polka.manager_is_polka_strict man) && (Box.manager_is_box man') then
    (
    printf "poly & box@.";
    box_meet_poly man' abs' man abs
    )
  ;
  if (Polka.manager_is_polka_strict man) && (Oct.manager_is_oct man') then
    (
    printf "poly & oct@.";
    oct_meet_poly man' abs' man abs
    )
  ;
  if (Polka.manager_is_polka_strict man) && (Polka.manager_is_polka_strict man') then
    (
    printf "poly & poly@.";
    abs_meet_abs man abs abs'
    )
;;

(**
 * Compute the reduced product of the two abstractions.
 *)
let reduced_product_print abs abs' =
  let man = Abstract1.manager abs in
  let man' = Abstract1.manager abs' in
  
  if (Box.manager_is_box man) && (Box.manager_is_box man') then
    printf "box & box@."
  ;
  if (Box.manager_is_box man) && (Oct.manager_is_oct man') then
    printf "box & oct@."
  ;
  if (Box.manager_is_box man) && (Polka.manager_is_polka_strict man') then
    printf "box & poly@."
  ;
  
  if (Oct.manager_is_oct man) && (Box.manager_is_box man') then
    printf "oct & box@."
  ;
  if (Oct.manager_is_oct man) && (Oct.manager_is_oct man') then
    printf "oct & oct@."
  ;
  if (Oct.manager_is_oct man) && (Polka.manager_is_polka_strict man') then
    printf "oct & poly@."
  ;
  
  if (Polka.manager_is_polka_strict man) && (Box.manager_is_box man') then
    printf "poly & box@."
  ;
  if (Polka.manager_is_polka_strict man) && (Oct.manager_is_oct man') then
    printf "poly & oct@."
  ;
  if (Polka.manager_is_polka_strict man) && (Polka.manager_is_polka_strict man') then
    printf "poly & poly@."
;;


(******************************************************************)
(*********************** Splitting operators **********************)
(******************************************************************)

(**
 * Split a polyhedra along a linear equation.
 *
 * Computes first the barycenter of a polyhedra, then computes the vector v
 * between the barycenter and the point the farthest from it and returns the
 * orthogonal vector of v.
 *)
let barycenter man abs =
  let gens = Abstract1.to_generator_array man abs in  
  let gen_env = gens.Generator1.array_env in
  (*print_gen gens gen_env;*)

  let size = Environment.size gen_env in
  let gen_float_array = gen_to_array gens size in
  let length = Array.length gen_float_array in
  
  (* Compute the barycenter *)
  let bary_tab = Array.make size 0. in
  let bary_tab' = Array.make size 0. in
  for i=0 to (length-1) do
    let gen_tab = gen_float_array.(i) in
    for j=0 to (size-1) do
      bary_tab.(j) <- bary_tab.(j) +. gen_tab.(j)
    done;
  done;
  for j=0 to (size-1) do
    bary_tab'.(j) <- bary_tab.(j);
    bary_tab.(j) <- bary_tab.(j) /. float_of_int length
  done;
  
  (* Get the farthest vertex from the barycenter wrt. the euclidian distance. *)
  let rec farthest i point_max i_max dist_max =
    if i >= length then
      (point_max, i_max, dist_max)
    else
      let dist_i = dist gen_float_array.(i) bary_tab in
      if dist_i > dist_max then
        farthest (i+1) gen_float_array.(i) i dist_i
      else
        farthest (i+1) point_max i_max dist_max
  in
  
  let (point_max, i_max, dist_max) = farthest 1 gen_float_array.(0) 0 (dist gen_float_array.(0) bary_tab) in
  let m = float_of_int length in
  
  (* let b = (b1, b2, ..., bn) the barycenter and p = (p1, p2, ..., pn) the farthest
   * point of b. The vector bp = (p1-b1, p2-b2, ..., pn-bn) and the orthogonal line 
   * to the vector bp passing by b has for equation:
   * (p1-b1)(x1-b1) + (p2-b2)(x2-b2) + ... + (pn-bn)(xn-bn) = 0
   *)
  let rec genere_linexpr i list cst =
    if i >= size then
      (list, cst)
    else
      let ci = m*.point_max.(i) -. bary_tab'.(i) in
      let cst' = cst +. (bary_tab'.(i) *. ci) in
      let ci' = m *. ci in
      let list' = List.append list [(Coeff.Scalar (Scalar.of_float ci'), Environment.var_of_dim gen_env i)] in
      genere_linexpr (i+1) list' cst'
  in
  
  let (list, cst) = genere_linexpr 0 [] 0. in
  let cst_sca = Scalar.of_float (-1. *.(cst +. split_prec)) in
  let linexp = Linexpr1.make gen_env in
  Linexpr1.set_list linexp list (Some (Coeff.Scalar cst_sca));
   
  linexp
;;


(**
 * Computes the largest domain of a box.
 *)
let rec largest tab i size i_max =
  if i == (Array.length tab) then
    (i_max, size)
  else
    let itv = tab.(i) in
    let size' = diam_interval itv in
    if Scalar.cmp size' size > 0 then
      largest tab (i+1) size' i
    else
      largest tab (i+1) size i_max
;;

(**
 * Split a box along its largest domain.
 *)
let largest_first tab env =
  let (index_var, size) = largest tab 1 (diam_interval tab.(0)) 0 in
  let var = Environment.var_of_dim env index_var in
  let value = mid_interval tab.(index_var) in
  let expr =  Linexpr1.make env in
  Linexpr1.set_list expr [(Coeff.s_of_int 1, var)] (Some (Coeff.Scalar (Scalar.neg value)));
  
  expr
;;


(**
 * Split an octagon
 *
 * For each basis bi compute the largest domain mi and splis the domain with the
 * smallest mi.
 *)
let split_octo man abs env =
  
  let dim = Environment.size env in
  let coeff1 = Coeff.s_of_int 1 in
  let coeff_1 = Coeff.s_of_int (-1) in
  
  (* Compute the max of the original basis *)
  let box = Abstract1.to_box man abs in
  let tab = box.Abstract1.interval_array in
  let (i_max, mmax) = largest tab 1 (diam_interval tab.(0)) 0 in
  
  (* Compute the max for the basis Bij *)
  let max_bij var_i var_j =
    let expr1 = Linexpr1.make env in
    Linexpr1.set_list expr1 [(coeff1, var_i) ; (coeff1, var_j)] None;
    let expr2 = Linexpr1.make env in
    Linexpr1.set_list expr2 [(coeff1, var_i) ; (coeff_1, var_j)] None;
    
    (* Get the interval for the expression "var_i + var_j" and "var_i -var_j" *)
    let itv1 = Abstract1.bound_linexpr man abs expr1 in
    let diam1 = diam_interval itv1 in
    let itv2 = Abstract1.bound_linexpr man abs expr2 in
    let diam2 = diam_interval itv2 in
    
    let cond = (Scalar.cmp diam1 diam2 > 0) in
    let diam_tmp = if cond then diam1 else diam2 in
    let expr_tmp = if cond then expr1 else expr2 in
    let itv_tmp = if cond then itv1 else itv2 in
    
    (expr_tmp, itv_tmp, diam_tmp)
  in
  
  (* For a given i, compute the min of all the max for the basis Bij 
   * min_{j \in \{1..n\}} (max Bij) *)
  let rec max_bi var_i j min_max cst linexpr =
    if j >= dim then
      (min_max, cst, linexpr)
    else
      let var_j = Environment.var_of_dim env j in
      let (expr_tmp, itv_tmp, diam_tmp) = max_bij var_i var_j in
      if Scalar.cmp diam_tmp min_max < 0 then
        max_bi var_i (j+1) diam_tmp (mid_interval itv_tmp) expr_tmp
      else
        max_bi var_i (j+1) min_max cst linexpr
  in
  
  (* Compute the min of all the max of the basis Bij 
   * min_{i \in \{1..n\}, j \in \{1..n\}} (max Bij) *)
  let rec max i min_max cst linexpr =
    if i >= dim-1 then
      (min_max, cst, linexpr)
    else
      let var_i = Environment.var_of_dim env i in
      let (min_max', cst', linexpr') = max_bi var_i (i+1) min_max cst linexpr in
      max (i+1) min_max' cst' linexpr'
  in
  
  let expr = Linexpr1.make env in
  Linexpr1.set_list expr [(coeff1, Environment.var_of_dim env i_max)] None;  
  let (min_max, cst, linexpr) = max 0 mmax (mid_interval tab.(i_max)) expr in
  Linexpr1.set_cst linexpr (Coeff.Scalar (Scalar.neg cst));
  
  linexpr
;;


(**
 * Split an abstract domain in two parts.
 *)
let split man abs env =
  
  (*let linexpr = 
  if Box.manager_is_box man then*)
    let box = Abstract1.to_box man abs in
    let tab = box.Abstract1.interval_array in
    let linexpr = largest_first tab env
  (*else
    if (Oct.manager_is_oct man) then
      split_octo man abs env
    else
      let box = Abstract1.to_box man abs in
      let tab = box.Abstract1.interval_array in
      largest_first tab env*)
      (*barycenter man abs*)
  in
  
  (*printf "split along: %a@.@." Linexpr1.print linexpr;*)
  
  (* Create the two constraints linexpr <= 0 and linexpr >=0 *)
  let cons = Lincons1.make (linexpr_neg linexpr env) Lincons1.SUPEQ in
  let cons' = Lincons1.make linexpr Lincons1.SUPEQ in
  let tab = Lincons1.array_make env 1 in
  Lincons1.array_set tab 0 cons;
  let abs1 = Abstract1.meet_lincons_array man abs tab in
  Lincons1.array_set tab 0 cons';
  let abs2 = Abstract1.meet_lincons_array man abs tab in
  
  [abs1; abs2]
;;


(******************************************************************)
(************************** Consistency ***************************)
(******************************************************************)

(**
 * One step of consistency on the conjonctions of dijonctions of constraints
 *)
let one_step_cons man abs0 env domains list =
  let abs = Abstract1.meet_lincons_array man abs0 domains in
  
  let rec union j abs' list' =
    if (j < (List.length list'))  && not (Abstract1.is_top man abs') then
      (
      let tab' = Tcons1.array_make env 1 in
      Tcons1.array_set tab' 0 (List.nth list' j);
      let abs'' = Abstract1.meet_tcons_array man abs tab' in
      Abstract1.join_with man abs' abs'';
      union (j+1) abs' list';
      )
    ;
  in
  
  let rec inter i =
    if (i < (List.length list)) && not (Abstract1.is_bottom man abs) then
      (
      let abs' = Abstract1.bottom man env in 
      let list' = List.nth list i in
      union 0 abs' list';
      Abstract1.meet_with man abs abs';
      inter (i+1);
      )
    ;
  in
  
  inter 0;
  
  abs
;;

(**
 * Apply the consistency on a set of conjuctions of dijonctions of constraints
 *)
let and_or_consistency man abs env list max_iter =
  
  let rec cons_loop abs n =
    (*printf "abs = %a@." Abstract1.print abs;*)
    if n >= max_iter then
      abs
    else
      if Abstract1.is_bottom man abs then
        abs
      else
        (
        let abs_tmp = Abstract1.copy man abs in
        
        let rec union j abs' list' =
          if (j < (List.length list'))  && not (Abstract1.is_top man abs') then
            (
            (*printf "abs' = %a@." Abstract1.print abs';
            printf "union c: %a@." Tcons1.print (List.nth list' j);*)
            let tab' = Tcons1.array_make env 1 in
            Tcons1.array_set tab' 0 (List.nth list' j);
            let abs'' = Abstract1.meet_tcons_array man abs tab' in
            Abstract1.join_with man abs' abs'';
            (*printf "-> %a@.@." Abstract1.print abs';*)
            union (j+1) abs' list';
            )
          ;
        in
        
        let rec inter i =
          if (i < (List.length list)) && not (Abstract1.is_bottom man abs) then
            (
            let abs' = Abstract1.bottom man env in 
            let list' = List.nth list i in
            union 0 abs' list';
            (*printf "inter abs' %a@." Abstract1.print abs';*)
            Abstract1.meet_with man abs abs';
            (*printf "-> %a@.@." Abstract1.print abs;*)
            inter (i+1);
            )
          ;
        in
        
        inter 0;
        
        if Abstract1.is_eq man abs_tmp abs then
          abs
        else
          cons_loop abs (n+1)
        )
  in
  
  let abs' = cons_loop abs 0 in
  abs'
;;


(******************************************************************)
(***************************** Search *****************************)
(******************************************************************)

(**
 * Search the space by splitting the domains in two.
 * The abstract domain abs must be different of bottom.
 *)
let rec find_all man abs env list max_iter prec nb_steps nb_sol =
  
  let abs' = and_or_consistency man abs env list max_iter in
  (*printf "@.abs = %a@." Abstract1.print abs';*)
  
  if Abstract1.is_bottom man abs' then
    (* No solutions in this sub-tree. *)
    (nb_steps, nb_sol)
  else
    (* Keep on searching in this sub-tree. *)
    let box = Abstract1.to_box man abs' in
    if is_small box prec then
      (* Solution found! *)
      (
      print_sol box;
      (nb_steps, nb_sol+1)
      )
    else
      (
      (*Abstract1.approximate man abs' 10;
      printf "@.abs_approx = %a@." Abstract1.print abs';*)
      (* Split the next variable, heuristic *)
      let list_abs = split man abs' env in
      
      List.fold_left (fun (nbe, nbsol) absi -> find_all man absi env list max_iter prec (nbe+1) nbsol) (nb_steps, nb_sol) list_abs
      )
;;

(*
 * Simulates the continuous search procedure in CP.
 *)
let search man env domains cons max_iter prec =
  
  let abs = Abstract1.of_lincons_array man env domains in
  if not (Abstract1.is_bottom man abs) then
    let (nb_steps, nb_sol) = find_all man abs env cons max_iter prec 1 0 in
    if nb_sol == 0 then
      printf "No solutions - #created nodes: %d@." nb_steps
    else
      if nb_sol == 1 then
        printf "Unique solution - #created nodes: %d@." nb_steps
      else
        printf "#solutions: %d - #created nodes: %d@." nb_sol nb_steps
  else
    printf "No Solutions - #created nodes: 0@."
  ;
;;

(**
 * Search the space by splitting the domains in two.
 * The abstract domain abs must be different of bottom.
 * Stops when it finds a solution.
 *)
let rec find_one man abs env list max_iter prec nb_steps nb_sol =
  
  if nb_sol > 0 then
    (nb_steps, nb_sol)
  else
    let abs' = and_or_consistency man abs env list max_iter in
    if Abstract1.is_bottom man abs' then
      (* No solutions in this sub-tree. *)
      (nb_steps+1, nb_sol)
    else
      (* Keep on searching in this sub-tree. *)
      let box = Abstract1.to_box man abs' in
      if is_small box prec then
        (* Solution found! *)
        (
        print_sol box;
        (nb_steps+1, nb_sol+1)
        )
      else
        (* Split the next variable, heuristic *)
        let list_abs = split man abs' env in
        
        List.fold_left (fun (nbe, nbsol) absi -> find_one man absi env list max_iter prec (nbe+1) nbsol) (nb_steps, nb_sol) list_abs
;;

(*
 * Search the space until it finds a solution.
 *)
let search_one man env domains cons max_iter prec =
  
  let abs = Abstract1.of_lincons_array man env domains in
  if not (Abstract1.is_bottom man abs) then
    let (nb_steps, nb_sol) = find_one man abs env cons max_iter prec 1 0 in
    if nb_sol == 0 then
      printf "No solutions - #created nodes: %d@." nb_steps
    else
        printf "#created nodes: %d@." nb_steps
  else
    printf "No Solutions - #created nodes: 0@."
  ;
;;



(******************************************************************)
(**************************** Problems ****************************)
(******************************************************************)

let x = Var.of_string "x";;
let y = Var.of_string "y";;
let z = Var.of_string "z";;
let x1 = Var.of_string "x1";;
let x2 = Var.of_string "x2";;
let x3 = Var.of_string "x3";;
let x4 = Var.of_string "x4";;
let x5 = Var.of_string "x5";;
let x6 = Var.of_string "x6";;
let x7 = Var.of_string "x7";;
let x8 = Var.of_string "x8";;
let x9 = Var.of_string "x9";;

(* One solution: x=0, y=1, z=2 *)
let eqlin =
  let env = Environment.make [||] [|x; y; z|] in
  let domains = Parser.lincons1_of_lstring env ["x>=-1000"; "x<=1000"; "y>=-1000"; "y<=1000"; "z>=-1000"; "z<=1000"] in
  let list = [["3*x+5*y-3*z+1=0"];["x-2*y+z=0"];["2*x+4*y+7*z-18=0"]] in
  let cons = List.map (List.map (Parser.tcons1_of_string env)) list in
  (env, domains, cons)
;;

(* Two solutions: y=0.618034, x=0.786151 ; y=0.618034, x=-0.786151 *)
let a =
  let env = Environment.make [||] [|x; y|] in
  let domains = Parser.lincons1_of_lstring env ["x>=-2"; "x<=2"; "y>=-2"; "y<=2"] in
  let list = [["x*x+y*y=1"];["x*x-y=0"]] in
  (*let list = [["2*x+y=0"];["x-2*y=0"]] in*)
  (*let list = [["(x-2)*(x-2)+(y-2)*(y-2)-1=0"];["(x-2)*(x-2)-(y-2)=0"]] in*)
  let cons = List.map (List.map (Parser.tcons1_of_string env)) list in
  (env, domains, cons)
;;

(* No solutions! *)
let appolonius =
  let env = Environment.make [||] [|x1; x2; x3; x4; x5; x6; x7; x8|] in
  let domains = Parser.lincons1_of_lstring env ["x1>=-1000"; "x1<=1000"; "x2>=-1000"; "x2<=1000"; "x3>=-1000"; "x3<=1000"; "x4>=-1000"; "x4<=1000"; "x5>=-1000"; "x5<=1000"; "x6>=-1000"; "x6<=1000"; "x7>=-1000"; "x7<=1000"; "x8>=-1000"; "x8<=1000"] in
  let list = [["2*x1=2"]; ["2*x2=1"]; ["2*x3=2"]; ["2*x4=1"]; ["2*x5-x6=0"]; ["x5+2*x6=2"]; ["(x1-x7)*(x1-x7)+x8*x8-x7*x7-(x8-x2)*(x8-x2)=0"]; ["(x1-x7)*(x1-x7)+x8*x8-(x3-x7)*(x3-x7)-(x4-x8)*(x4-x8)=0"]; ["x1*x1+x2*x2-2*x1*x7+2*x2*x8=0"]; ["x1*x1-x3*x3*x3-x4*x4-2*x7*(x1-x3)+2*x4*x8=0"]] in
  let cons = List.map (List.map (Parser.tcons1_of_string env)) list in
  (env, domains, cons)
;;

(* 8 solutions *)
let bellido =
  let env = Environment.make [||] [|x1; x2; x3; x4; x5; x6; x7; x8; x9|] in
  let domains = Parser.lincons1_of_lstring env ["x1>=-1000"; "x1<=1000"; "x2>=-1000"; "x2<=1000"; "x3>=-1000"; "x3<=1000"; "x4>=-1000"; "x4<=1000"; "x5>=-1000"; "x5<=1000"; "x6>=-1000"; "x6<=1000"; "x7>=-1000"; "x7<=1000"; "x8>=-1000"; "x8<=1000"; "x9>=-1000"; "x9<=1000"] in
  let list = [["(x1-6)*(x1-6)+x2*x2+x3*x3=104"]; ["x4*x4+(x5-6)*(x5-6)+x6*x6=104"]; ["x7*x7+(x8-12)*(x8-12)+(x9-6)*(x9-6)=80"]; ["x1*(x4-6)+x5*(x2-6)+x3*x6=52"]; ["x1*(x7-6)+x8*(x2-12)+x9*(x3-6)=-64"]; ["x4*x7+x8*(x5-12)+x9*(x6-6)-6*x5=-32"]; ["2*x2+2*x3-2*x6-x4-x5-x7-x9=-18"]; ["x1+x2+2*x3+2*x4+2*x6-2*x7+x8-x9=38"]; ["x1+x3+x5-x6+2*x7-2*x8-2*x4=-8"]] in
  let cons = List.map (List.map (Parser.tcons1_of_string env)) list in
  (env, domains, cons)
;;

(* Two solutions: y=1, x=1 ; y=1, x=-1 *)
let entier =    
  let env = Environment.make [|x; y|] [||] in
  let domains = Parser.lincons1_of_lstring env ["x>=-3"; "x<=3"; "y>=-3"; "y<=3"] in
  (*let domains = Parser.lincons1_of_lstring env ["x>=-3"; "x<=2"; "y>=-3"; "y<=2"; "x+y>=-1"; "3y-6x<=-1"; "x+y<=2"; "6y-3x>=-2"] in*)
  let cons = List.map (List.map (Parser.tcons1_of_string env)) [["x*x-y=0"];["x*x+y-2=0"]] in
  (env, domains, cons)
;;

(* Two solutions: y=1, x=1 ; y=1, x=-1 *)
let reel =    
  let env = Environment.make [||] [|x; y|] in
  let domains = Parser.lincons1_of_lstring env ["x>=-3"; "x<=3"; "y>=-3"; "y<=3"] in
  (*let domains = Parser.lincons1_of_lstring env ["x>=-3"; "x<=2"; "y>=-3"; "y<=2"; "x+y>=-1"; "3y-6x<=-1"; "x+y<=2"; "6y-3x>=-2"] in*)
  let cons = List.map (List.map (Parser.tcons1_of_string env)) [["x*x-y=0"];["x*x+y-2=0"]] in
  (env, domains, cons)
;;

(* Two solutions: y=1, x=1 ; y=1, x=-1 *)
let mixte =    
  let env = Environment.make [|x|] [|y|] in
  let domains = Parser.lincons1_of_lstring env ["x>=-3"; "x<=3"; "y>=-3"; "y<=3"] in
  (*let domains = Parser.lincons1_of_lstring env ["x>=-3"; "x<=2"; "y>=-3"; "y<=2"; "x+y>=-1"; "3y-6x<=-1"; "x+y<=2"; "6y-3x>=-2"] in*)
  let cons = List.map (List.map (Parser.tcons1_of_string env)) [["x*x-y=0"];["x*x+y-2=0"]] in
  (env, domains, cons)
;;



(******************************************************************)
(****************************** Main ******************************)
(******************************************************************)

(*let main =
  if (Array.length Sys.argv) < 2 then
    printf "To solve a continuous problem type:@.  ./continu.opt man [prec] [max]@.  man\tthe abstract domain manager you want to use:@.\t0: polka@.\t1: box@.\t2: octagons@.  prec\tthe the precision you want to reach.@.\tprec is optional and set by default to 0.1.@.  max\tthe maximum number of iterations you want to perform during the consistency.@.\tmax is optional and set by default to 10.@."
  else
    (
    let m = int_of_string Sys.argv.(1) in
    let prec = ref (Mpqf.of_float 0.01) in
    if (Array.length Sys.argv) >= 3 then
      prec := Mpqf.of_float (float_of_string Sys.argv.(2))
    ;
    printf "prec: %a@." Mpqf.print !prec;
    let max_iter = ref 10 in
    if (Array.length Sys.argv) >= 4 then
      max_iter := int_of_string Sys.argv.(3)
    ;
    printf "max: %d@." !max_iter;
    
    let change man funid =
      let s = Manager.get_funopt man funid in
      let s' = {s with Manager.algorithm = 1;} in
      Manager.set_funopt man funid s';
    in
    
    let doit man =
      (*change man Manager.Funid_approximate;
      change man Manager.Funid_meet;
      change man Manager.Funid_meet_array;
      change man Manager.Funid_meet_lincons_array;
      change man Manager.Funid_meet_tcons_array;
      change man Manager.Funid_join;*)
      (*printf "man: %s@." (Manager.get_library man);
      printf "@.-------------- eqlin --------------@.";
      let (env_eqlin, domains_eqlin, cons_eqlin) = eqlin in
      search man env_eqlin domains_eqlin cons_eqlin !max_iter !prec;*)
      printf "@.-------------- a --------------@.";
      let (env_a, domains_a, cons_a) = a in
      search man env_a domains_a cons_a !max_iter !prec;
      printf "@.-------------- appolionus --------------@.";
      let (env_appo, domains_appo, cons_appo) = appolonius in
      search man env_appo domains_appo cons_appo !max_iter !prec;
      (*printf "@.-------------- bellido --------------@.";
      let (env_bellido, domains_bellido, cons_bellido) = bellido in
      search man env_bellido domains_bellido cons_bellido !max_iter !prec;
      printf "@.-------------- entier --------------@.";
      let (env_entier, domains_entier, cons_entier) = entier in
      search man env_entier domains_entier cons_entier !max_iter !prec;
      printf "@.-------------- reel --------------@.";
      let (env_reel, domains_reel, cons_reel) = reel in
      search man env_reel domains_reel cons_reel !max_iter !prec;
      printf "@.-------------- mixte --------------@.";
      let (env_mixte, domains_mixte, cons_mixte) = mixte in
      search man env_mixte domains_mixte cons_mixte !max_iter !prec;*)
    in
    
    match m with
    | 0 -> 
      let man = Polka.manager_alloc_strict() in
      let internal = Polka.manager_get_internal man in
      Polka.set_approximate_max_coeff_size internal 20;
      doit man
    | 1 -> doit (Box.manager_alloc ())
    | 2 -> doit (Oct.manager_alloc ())
    | _ -> failwith "The manager argument must be in [0, 6], type ./continu.opt for more info."
    )
;;*)

let test_conversions =
  let manpoly = Polka.manager_alloc_strict() in
  let manbox = Box.manager_alloc () in
  let manoct = Oct.manager_alloc () in
  
  let box_env = Environment.make [||] [|x; y|] in
  let box_cons = Parser.tcons1_of_lstring box_env ["x>=-3"; "x<=3"; "y>=-3"; "y<=3"; "x*x-y=0"; "x*x+y-2=0"] in
  let box = Abstract1.of_tcons_array manbox box_env box_cons in
  printf "box = %a@." Abstract1.print box;
  printf "box_bounds = %a@." (print_array Interval.print) (Abstract1.to_box manbox box).Abstract1.interval_array;
  
  let poly_env = Environment.make [||] [|x1; y; z|] in
  let poly_cons = Parser.tcons1_of_lstring poly_env ["y>=-3"; "y<=3"; "z>=-5"; "z<=5"; "3*x1+5*y-3*z+1=0"; "x1-2*y+z=0"] in
  let poly = Abstract1.of_tcons_array manpoly poly_env poly_cons in
  printf "poly = %a@." Abstract1.print poly;
  printf "poly_bounds = %a@." (print_array Interval.print) (Abstract1.to_box manpoly poly).Abstract1.interval_array;
  
  let oct_env = Environment.make [||] [|x; z|] in
  let oct_cons = Parser.tcons1_of_lstring oct_env ["x>=-3"; "x<=3"; "z>=-5"; "z<=5"; "2*x+z>=0"; "-2*x+z>=0"] in
  let oct = Abstract1.of_tcons_array manoct oct_env oct_cons in
  Abstract1.meet_tcons_array_with manoct oct oct_cons;
  printf "oct = %a@." Abstract1.print oct;
  printf "oct_bounds = %a@." (print_array Interval.print) (Abstract1.to_box manoct oct).Abstract1.interval_array;
  
  (* Produit réduit box-poly *)
  printf "@.---------- box and poly ----------@.";
  box_meet_poly manbox box manpoly poly;
  reduced_product_print box poly;
  reduced_product_print poly box;
  reduced_product box poly;
  (*reduced_product poly box;*)
  printf "box = %a@." Abstract1.print box;
  printf "box_bounds = %a@." (print_array Interval.print) (Abstract1.to_box manbox box).Abstract1.interval_array;
  printf "poly = %a@." Abstract1.print poly;
  printf "poly_bounds = %a@." (print_array Interval.print) (Abstract1.to_box manpoly poly).Abstract1.interval_array;
  
  (* Produit réduit oct-poly *)
  printf "@.---------- oct and poly ----------@.";
  oct_meet_poly manoct oct manpoly poly;
  reduced_product_print oct poly;
  reduced_product_print poly oct;
  (*reduced_product oct poly;
  reduced_product poly oct;*)
  printf "oct = %a@." Abstract1.print oct;
  printf "oct_bounds = %a@." (print_array Interval.print) (Abstract1.to_box manoct oct).Abstract1.interval_array;
  printf "poly = %a@." Abstract1.print poly;
  printf "poly_bounds = %a@." (print_array Interval.print) (Abstract1.to_box manpoly poly).Abstract1.interval_array;
  
  (* Produit réduit oct-box *)
  printf "@.---------- box and oct ----------@.";
  box_meet_oct manbox box manoct oct;
  reduced_product_print box oct;
  reduced_product_print oct box;
  (*reduced_product box oct;
  reduced_product oct box;*)
  printf "box = %a@." Abstract1.print box;
  printf "box_bounds = %a@." (print_array Interval.print) (Abstract1.to_box manbox box).Abstract1.interval_array;
  printf "oct = %a@." Abstract1.print oct;
  printf "oct_bounds = %a@." (print_array Interval.print) (Abstract1.to_box manoct oct).Abstract1.interval_array;
;;
