open Apron;;
open Mpqf;;
open Format;;

let split_prec = 0.000001
let split_prec_mpqf = Mpqf.of_float split_prec

let sqrt2 = 0.707106781186548
let sqrt2_mpqf = Mpqf.of_float sqrt2

let print_array = Abstract0.print_array
let lincons1_array_print fmt x =
  Lincons1.array_print fmt x

let generator1_array_print fmt x =
  Generator1.array_print fmt x

(*
 * Different conversion operators 
 *)
let scalar_to_mpqf = function
  | Scalar.Mpqf x -> x
  | Scalar.Float x -> Mpqf.of_float x
  | Scalar.Mpfrf x -> Mpfrf.to_mpqf x

let scalar_to_float = function
  | Scalar.Mpqf x -> Mpqf.to_float x
  | Scalar.Float x -> x
  | Scalar.Mpfrf x -> Mpfrf.to_float ~round:Mpfr.Near x

let coeff_to_float = function
  | Coeff.Scalar x -> scalar_to_float x
  | Coeff.Interval i -> scalar_to_float i.Interval.inf

(* Compute the sum of two scalars *)
let scalar_add sca sca' = 
  let value = scalar_to_mpqf sca in
  let value' = scalar_to_mpqf sca' in
  let sum = Mpqf.add value value' in
  Scalar.of_mpqf sum

(* Compute the sum of two scalars *)
let scalar_mul_sqrt2 sca = 
  let value = scalar_to_mpqf sca in
  let mult = Mpqf.mul value sqrt2_mpqf in
  Scalar.of_mpqf mult

(* Compute the sum of a scalar and a Mpqf *)
let scalar_plus_mpqf sca mpqf = 
  let value = scalar_to_mpqf sca in
  let sum = Mpqf.add value mpqf in
  Scalar.of_mpqf sum

let meet_linexpr abs man env expr =
  let cons = Lincons1.make expr Lincons1.SUPEQ in
  let tab = Lincons1.array_make env 1 in
  Lincons1.array_set tab 0 cons;
  let abs' = Abstract1.meet_lincons_array man abs tab in
  abs'

let diam inf sup =
  let mpqf_inf = scalar_to_mpqf inf in
  let mpqf_sup = scalar_to_mpqf sup in
  Mpqf.sub mpqf_sup mpqf_inf

let diam_interval itv =
  diam itv.Interval.inf itv.Interval.sup

let mid inf sup = 
  let mpqf_inf = scalar_to_mpqf inf in
  let mpqf_sup = scalar_to_mpqf sup in
  Scalar.of_mpqf (Mpqf.add (Mpqf.div (Mpqf.add mpqf_inf mpqf_sup) (Mpqf.of_int 2)) split_prec_mpqf)

let mid_interval itv =
  mid itv.Interval.inf itv.Interval.sup

let rec largest tab i max i_max =
  if i>=Array.length tab then
    (
    (*printf "\t->%f@.@." (Mpqf.to_float max);*)
    (max, i_max)
    )
  else
    (
    (*printf "%f " (Mpqf.to_float max);*)
    let dim = diam_interval (tab.(i)) in
    if Mpqf.cmp dim max > 0 then
      (
      (*printf "< %f@." (Mpqf.to_float dim);*)
      largest tab (i+1) dim i
      )
    else
      (
      (*printf "> %f@." (Mpqf.to_float dim);*)
      largest tab (i+1) max i_max
      )
    )

(** Compute the minimal and the maximal diameter of an array on intervals *)
let rec minmax tab i max i_max min i_min =
  if i>=Array.length tab then
    (max, i_max, min, i_min)
  else
    let dim = diam_interval (tab.(i)) in
    if Mpqf.cmp dim max > 0  then
      minmax tab (i+1) dim i min i_min
    else
      if Mpqf.cmp min dim > 0 then
        minmax tab (i+1) max i_max dim i
      else
        minmax tab (i+1) max i_max min i_min

(** Converts a Generator0 into an array of floats. *)
let to_float_array gen size =
  let tab = Array.make size 0. in
  let gen_lin = gen.Generator0.linexpr0 in
  for i=0 to (size-1) do
    let coeff = Linexpr0.get_coeff gen_lin i in
    tab.(i) <- coeff_to_float coeff
  done;
  tab

(** Converts a Generator1 into an array of array of floats. *)
let gen_to_array gens size =
  let gen_tab = gens.Generator1.generator0_array in
  let tab = Array.make (Array.length gen_tab) (Array.make size 0.) in
  for i=0 to ((Array.length gen_tab)-1) do
    tab.(i) <- to_float_array gen_tab.(i) size
  done;
  tab
  
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

(** Compute the maximal distance between a point and an array of points. 
 * A point correspond to an array of floats.
 *)
let maxdist point tabpoints =
  let length = Array.length tabpoints in
  
  let rec maxd i point_max i_max dist_max =
    if i >= length then
      (point_max, i_max, dist_max)
    else
      let dist_i = dist tabpoints.(i) point in
      if dist_i > dist_max then
        maxd (i+1) tabpoints.(i) i dist_i
      else
        maxd (i+1) point_max i_max dist_max
  in
  
  let (point_max, i_max, dist_max) = maxd 1 tabpoints.(0) 0 (dist tabpoints.(0) point) in
  
  (point_max, i_max, dist_max)

(** Compute the maximal distance between two points in an array of points. 
 * A point correspond to an array of floats.
 *)
let maxdisttab tabpoints =
  let length = Array.length tabpoints in
  
  let rec maxd i p1 i1 p2 i2 dist_max =
    if i >= length then
      (p1, i1, p2, i2, dist_max)
    else
      let tabpoints' = Array.sub tabpoints (i+1) (length-i-1) in
      let (pj, j, dist) = maxdist tabpoints.(i) tabpoints' in
      if dist > dist_max then
        maxd (i+1) tabpoints.(i) i pj j dist
      else
        maxd (i+1) p1 i1 p2 i2 dist_max
  in
  let (p0, i0, dist) = maxdist tabpoints.(0) (Array.sub tabpoints 1 (length-1)) in
  let (p1, i1, p2, i2, dist_max) = maxd 1 tabpoints.(0) 0 p0 i0 dist in
  (p1, i1, p2, i2, dist_max)

(* let p1 = (p11, p12, ..., p1n) and p2 = (p21, p22, ..., p2n) two points
 * The vector p1p2 is (p21-p11, p22-p12, ..., p2n-p1n) and the orthogonal line 
 * to the vector p1p2 passing by the center of the vector has for equation:
 * (p21-p11)(x1-b1) + (p22-p12)(x2-b2) + ... + (p2n-p1n)(xn-bn) = 0
 * with b = ((p11+p21)/2, (p12+p22)/2, ..., (p1n+p2n)/2)
 *)
let rec genere_linexpr gen_env size p1 p2 i list1 list2 cst =
  if i >= size then
    (list1, list2, cst)
  else
    let ci = p2.(i) -. p1.(i) in
    let cst' = cst +. ((p1.(i) +. p2.(i)) *. ci) in
    let ci' = 2. *. ci in
    let coeffi = Coeff.Scalar (Scalar.of_float ci') in
    let list1' = List.append list1 [(coeffi, Environment.var_of_dim gen_env i)] in
    let list2' = List.append list2 [(Coeff.neg coeffi, Environment.var_of_dim gen_env i)] in
    genere_linexpr gen_env size p1 p2 (i+1) list1' list2' cst'

(** 
 * Module for Abstract Domains for Constraint Programming.
 * These are abstract domains with consistency, split and precision operators.
 *)
module type AbstractCP =
 sig
  type t
  val of_lincons_array : Environment.t -> Lincons1.earray -> t Abstract1.t
  val get_manager : t Manager.t
  val is_small : t Abstract1.t -> float -> (bool * Linexpr1.t list)
  val split : t Abstract1.t -> Linexpr1.t list -> t Abstract1.t list
 end
 
(** 
 * Module for the Box Abstract Domains for Constraint Programming.
 *)
module BoxCP : AbstractCP =
  struct
    type t = Box.t
    let man = Box.manager_alloc ()
    let of_lincons_array env domains =
      let abs = Abstract1.of_lincons_array man env domains in
      abs
    let get_manager =
      man
    let is_small boxad prec =
      let env = Abstract1.env boxad in
      let box = Abstract1.to_box man boxad in
      let itv = box.Abstract1.interval_array in
      let (max, i_max) = largest itv 1 (diam_interval itv.(0)) 0 in
      let dim = Mpqf.to_float max in
      let mid = mid_interval itv.(i_max) in
      let var = Environment.var_of_dim env i_max in
      let typ_var = Environment.typ_of_var env var in
      let value = if typ_var == Environment.INT then (scalar_plus_mpqf mid split_prec_mpqf) else mid in
      (* var <= mid*)
      let expr =  Linexpr1.make env in
      Linexpr1.set_list expr [(Coeff.s_of_int (-1), var)] (Some (Coeff.Scalar (mid)));
      (* var >= value*)
      let expr' =  Linexpr1.make env in
      Linexpr1.set_list expr' [(Coeff.s_of_int 1, var)] (Some (Coeff.Scalar (Scalar.neg value)));
      (dim <= prec, [expr; expr'])
    let split boxad list =
      let env = Abstract1.env boxad in
      let abs1 = meet_linexpr boxad man env (List.nth list 0) in
      let abs2 = meet_linexpr boxad man env (List.nth list 1) in
      [abs1; abs2]
  end
 
(** 
 * Module for the Octagon Abstract Domains for Constraint Programming.
 *)
module OctMinMinCP : AbstractCP =
  struct
    type t = Oct.t
    let man = Oct.manager_alloc ()
    let of_lincons_array env domains =
      let abs = Abstract1.of_lincons_array man env domains in
      abs
    let get_manager =
      man
    let is_small octad prec =
      (*printf "oct = %a@." Abstract1.print octad;*)
      let env = Abstract1.env octad in
      let dim = Environment.size env in
      let coeff1 = Coeff.s_of_int 1 in
      let coeff_1 = Coeff.s_of_int (-1) in
      
      (* Compute the max and the min for the basis Bij *)
      let minmax_bij var_i var_j =
        let expr1 = Linexpr1.make env in
        Linexpr1.set_list expr1 [(coeff1, var_i) ; (coeff1, var_j)] None;
        let expr2 = Linexpr1.make env in
        Linexpr1.set_list expr2 [(coeff1, var_i) ; (coeff_1, var_j)] None;
        
        let expr1' = Linexpr1.make env in
        Linexpr1.set_list expr1' [(coeff_1, var_i) ; (coeff_1, var_j)] None;
        let expr2' = Linexpr1.make env in
        Linexpr1.set_list expr2' [(coeff_1, var_i) ; (coeff1, var_j)] None;
        
        (* Get the interval for the expression "vi + vj" and "vi -vj" *)
        let itv1 = Abstract1.bound_linexpr man octad expr1 in
        let diam1 = Scalar.of_mpqf (diam_interval itv1) in
        let itv2 = Abstract1.bound_linexpr man octad expr2 in
        let diam2 = Scalar.of_mpqf (diam_interval itv2) in
        
        let cond = (Scalar.cmp diam1 diam2 > 0) in
        let diam_max = if cond then (scalar_mul_sqrt2 diam1) else (scalar_mul_sqrt2 diam2) in
        let expr_max = if cond then expr1 else expr2 in
        let expr_max' = if cond then expr1' else expr2' in
        let itv_max = if cond then itv1 else itv2 in
        let diam_min = if cond then (scalar_mul_sqrt2 diam2) else (scalar_mul_sqrt2 diam1) in
        (*printf "      min = %f, max = %f@." (scalar_to_float diam_min) (scalar_to_float diam_max);*)
        (expr_max, expr_max', itv_max, diam_max, diam_min)
      in
      
      (* For a given i, compute the min of all the min for the basis Bij 
       * and the corresopnding max.
       * min_{j \in \{i+1...n\}} (min Bij) *)
      let rec minmax_bi var_i j min max cst linexpr1 linexpr2 =
        if j >= dim then
          (linexpr1, linexpr2, cst, max, min)
        else
          (
          let var_j = Environment.var_of_dim env j in
          let (expr_max, expr_max', itv_max, diam_max, diam_min) = minmax_bij var_i var_j in
          (*printf "    min = %f, max = %f, min' = %f, max' = %f@." (scalar_to_float min) (scalar_to_float max) (scalar_to_float diam_min) (scalar_to_float diam_max);*)
          if Scalar.cmp diam_min min < 0 then
            minmax_bi var_i (j+1) diam_min diam_max (mid_interval itv_max) expr_max expr_max'
          else
            minmax_bi var_i (j+1) min max cst linexpr1 linexpr2
          )
      in
   
      (* Compute the min of all the min of the basis Bij and the corresponding max
       * min_{i \in \{1...n\}, j \in \{i+1...n\}} (min Bij) *)
      let rec minmax_b i min max cst linexpr1 linexpr2 =
        if i >= dim-1 then
          (linexpr1, linexpr2, cst, max, min)
        else
          (
          (*printf "[%a ; %a], %f" Linexpr1.print linexpr1 Linexpr1.print linexpr2 (scalar_to_float min_max);*)
          let var_i = Environment.var_of_dim env i in
          let (linexpr1', linexpr2', cst', max', min') = minmax_bi var_i (i+1) min max cst linexpr1 linexpr2 in
          (*printf "  min = %f, max = %f, min' = %f, max' = %f@." (scalar_to_float min) (scalar_to_float max) (scalar_to_float min') (scalar_to_float max');*)
          if Scalar.cmp min' min < 0 then
            (
            (* printf " > %f, [%a ; %a]@." (scalar_to_float min_max') Linexpr1.print linexpr1' Linexpr1.print linexpr2';*)
            minmax_b (i+1) min' max' cst' linexpr1' linexpr2'
            )
          else
            (
            (* printf " < %f, [%a ; %a]@." (scalar_to_float min_max') Linexpr1.print linexpr1' Linexpr1.print linexpr2';*)
            minmax_b (i+1) min max cst linexpr1 linexpr2
            )
          )
      in
      
      let box = Abstract1.to_box man octad in
      let tab = box.Abstract1.interval_array in
      (*printf "box = %a@." (print_array Interval.print) tab;*)
      let (mmax, i_max, mmin, i_min) = minmax tab 1 (diam_interval tab.(0)) 0 (diam_interval tab.(0)) 0 in
      (*printf "min = %f, max = %f@." (Mpqf.to_float mmin) (Mpqf.to_float mmax);*)
      
      let mmax' = Scalar.of_mpqf mmax in
      let expr1 = Linexpr1.make env in
      Linexpr1.set_list expr1 [(coeff1, Environment.var_of_dim env i_max)] None;
      let expr2 = Linexpr1.make env in
      Linexpr1.set_list expr2 [(coeff_1, Environment.var_of_dim env i_max)] None;
      let mmin' = Scalar.of_mpqf mmin in
      let (linexpr1, linexpr2, cst, max, min) = minmax_b 0 mmin' mmax' (mid_interval tab.(i_max)) expr1 expr2 in
      (*printf "min = %f, max = %f@." (scalar_to_float min) (scalar_to_float max);*)
      
      let sca_cst = scalar_plus_mpqf cst split_prec_mpqf in
      Linexpr1.set_cst linexpr1 (Coeff.Scalar (Scalar.neg sca_cst));
      Linexpr1.set_cst linexpr2 (Coeff.Scalar sca_cst);
      
      let max = scalar_to_float max in
      (*printf "max = %f@.split = [%a, %a]@." max Linexpr1.print linexpr1 Linexpr1.print linexpr2;*)
      (max <= prec, [linexpr1; linexpr2])
    let split octad list =
      let env = Abstract1.env octad in
      let abs1 = meet_linexpr octad man env (List.nth list 0) in
      let abs2 = meet_linexpr octad man env (List.nth list 1) in
      [abs1; abs2]
  end

(** 
 * Module for the Octagon Abstract Domains for Constraint Programming.
 *)
module OctMinMaxCP : AbstractCP =
  struct
    type t = Oct.t
    let man = Oct.manager_alloc ()
    let of_lincons_array env domains =
      let abs = Abstract1.of_lincons_array man env domains in
      abs
    let get_manager =
      man
    let is_small octad prec =
      (*printf "oct = %a@." Abstract1.print octad;*)
      let env = Abstract1.env octad in
      let dim = Environment.size env in
      let coeff1 = Coeff.s_of_int 1 in
      let coeff_1 = Coeff.s_of_int (-1) in
      
      (* Compute the max for the basis Bij *)
      let max_bij var_i var_j =
        let expr1 = Linexpr1.make env in
        Linexpr1.set_list expr1 [(coeff1, var_i) ; (coeff1, var_j)] None;
        let expr2 = Linexpr1.make env in
        Linexpr1.set_list expr2 [(coeff1, var_i) ; (coeff_1, var_j)] None;
        
        let expr1' = Linexpr1.make env in
        Linexpr1.set_list expr1' [(coeff_1, var_i) ; (coeff_1, var_j)] None;
        let expr2' = Linexpr1.make env in
        Linexpr1.set_list expr2' [(coeff_1, var_i) ; (coeff1, var_j)] None;
        
        (* Get the interval for the expression "vi + vj" and "vi -vj" *)
        let itv1 = Abstract1.bound_linexpr man octad expr1 in
        let diam1 = Scalar.of_mpqf (diam_interval itv1) in
        let itv2 = Abstract1.bound_linexpr man octad expr2 in
        let diam2 = Scalar.of_mpqf (diam_interval itv2) in
        
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
            max_bi var_i (j+1) diam_tmp (mid_interval itv_tmp) expr_tmp1 expr_tmp2
          else
            max_bi var_i (j+1) min_max cst linexpr1 linexpr2
      in
   
      (* Compute the min of all the max of the basis Bij 
       * min_{i \in \{1...n\}, j \in \{i+1...n\}} (max Bij) *)
      let rec max i min_max cst linexpr1 linexpr2 =
        if i >= dim-1 then
          (linexpr1, linexpr2, cst, min_max)
        else
          (
          (*printf "[%a ; %a], %f" Linexpr1.print linexpr1 Linexpr1.print linexpr2 (scalar_to_float min_max);*)
          let var_i = Environment.var_of_dim env i in
          let (linexpr1', linexpr2', cst', min_max') = max_bi var_i (i+1) min_max cst linexpr1 linexpr2 in
          if Scalar.cmp min_max' min_max < 0 then
            (
            (* printf " > %f, [%a ; %a]@." (scalar_to_float min_max') Linexpr1.print linexpr1' Linexpr1.print linexpr2';*)
            max (i+1) min_max' cst' linexpr1' linexpr2'
            )
          else
            (
            (* printf " < %f, [%a ; %a]@." (scalar_to_float min_max') Linexpr1.print linexpr1' Linexpr1.print linexpr2';*)
            max (i+1) min_max cst linexpr1 linexpr2
            )
          )
      in
      
      let box = Abstract1.to_box man octad in
      let tab = box.Abstract1.interval_array in
      (*printf "box = %a@." (print_array Interval.print) tab;*)
      let (mmax, i_max) = largest tab 1 (diam_interval tab.(0)) 0 in
      let mmax' = Scalar.of_mpqf mmax in
      let expr1 = Linexpr1.make env in
      Linexpr1.set_list expr1 [(coeff1, Environment.var_of_dim env i_max)] None;
      let expr2 = Linexpr1.make env in
      Linexpr1.set_list expr2 [(coeff_1, Environment.var_of_dim env i_max)] None;
      let (linexpr1, linexpr2, cst, min_max) = max 0 mmax' (mid_interval tab.(i_max)) expr1 expr2 in
      
      let sca_cst = scalar_plus_mpqf cst split_prec_mpqf in
      Linexpr1.set_cst linexpr1 (Coeff.Scalar (Scalar.neg sca_cst));
      Linexpr1.set_cst linexpr2 (Coeff.Scalar sca_cst);
      
      let max = scalar_to_float min_max in
      (*printf "max = %f@.split = [%a, %a]@." max Linexpr1.print linexpr1 Linexpr1.print linexpr2;*)
      (max <= prec, [linexpr1; linexpr2])
    let split octad list =
      let env = Abstract1.env octad in
      let abs1 = meet_linexpr octad man env (List.nth list 0) in
      let abs2 = meet_linexpr octad man env (List.nth list 1) in
      [abs1; abs2]
  end

 
(** 
 * Module for the Octagon Abstract Domains for Constraint Programming.
 *)
module OctBoxCP : AbstractCP =
  struct
    type t = Oct.t
    let man = Oct.manager_alloc ()
    let of_lincons_array env domains =
      let abs = Abstract1.of_lincons_array man env domains in
      abs
    let get_manager =
      man
    let is_small octad prec =let env = Abstract1.env octad in
      let box = Abstract1.to_box man octad in
      let itv = box.Abstract1.interval_array in
      let (max, i_max) = largest itv 1 (diam_interval itv.(0)) 0 in
      let dim = Mpqf.to_float max in
      let mid = mid_interval itv.(i_max) in
      let var = Environment.var_of_dim env i_max in
      let typ_var = Environment.typ_of_var env var in
      let value = if typ_var == Environment.INT then (scalar_plus_mpqf mid split_prec_mpqf) else mid in
      (* var <= mid*)
      let expr =  Linexpr1.make env in
      Linexpr1.set_list expr [(Coeff.s_of_int (-1), var)] (Some (Coeff.Scalar (mid)));
      (* var >= value*)
      let expr' =  Linexpr1.make env in
      Linexpr1.set_list expr' [(Coeff.s_of_int 1, var)] (Some (Coeff.Scalar (Scalar.neg value)));
      (dim <= prec, [expr; expr'])
    let split octad list =
      let env = Abstract1.env octad in
      let abs1 = meet_linexpr octad man env (List.nth list 0) in
      let abs2 = meet_linexpr octad man env (List.nth list 1) in
      [abs1; abs2]
  end

(** 
 * Module for the Polyhedron Abstract Domains for Constraint Programming.
 *)
module PolyCP : AbstractCP =
  struct
    type t = Polka.strict Polka.t
    let man = Polka.manager_alloc_strict()
    let of_lincons_array env domains =
      let abs = Abstract1.of_lincons_array man env domains in
      abs
    let get_manager =
      man
    let is_small polyad prec =
      let poly = Abstract1.to_generator_array man polyad in 
      let gen_env = poly.Generator1.array_env in
      (*print_gen gens gen_env;*)
      let size = Environment.size gen_env in
      let gen_float_array = gen_to_array poly size in
      let (p1, i1, p2, i2, dist_max) = maxdisttab gen_float_array in
      let (list1, list2, cst) = genere_linexpr gen_env size p1 p2 0 [] [] 0. in
      let cst_sca1 = Scalar.of_float (-1. *.(cst +. split_prec)) in
      let cst_sca2 = Scalar.of_float (cst +. split_prec) in
      let linexp = Linexpr1.make gen_env in
      Linexpr1.set_list linexp list1 (Some (Coeff.Scalar cst_sca1));
      let linexp' = Linexpr1.make gen_env in
      Linexpr1.set_list linexp' list2 (Some (Coeff.Scalar cst_sca2));
      (dist_max <= prec, [linexp; linexp'])
    let split polyad list =
      let env = Abstract1.env polyad in
      let abs1 = meet_linexpr polyad man env (List.nth list 0) in
      let abs2 = meet_linexpr polyad man env (List.nth list 1) in
      [abs1; abs2]
  end