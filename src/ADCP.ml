open Apron
open Mpqf
open Format
open Utils

let meet_linexpr abs man env expr =
  let cons = Lincons1.make expr Lincons1.SUPEQ in
  let tab = Lincons1.array_make env 1 in
  Lincons1.array_set tab 0 cons;
  let abs' = Abstract1.meet_lincons_array man abs tab in
  abs'

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

(* Compute the minimal and the maximal diameter of an array on intervals *)
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

(* let p1 = (p11, p12, ..., p1n) and p2 = (p21, p22, ..., p2n) two points
 * The vector p1p2 is (p21-p11, p22-p12, ..., p2n-p1n) and the orthogonal line 
 * to the vector p1p2 passing by the center of the vector has for equation:
 * (p21-p11)(x1-b1) + (p22-p12)(x2-b2) + ... + (p2n-p1n)(xn-bn) = 0
 * with b = ((p11+p21)/2, (p12+p22)/2, ..., (p1n+p2n)/2)
 *)
let rec genere_linexpr gen_env size p1 p2 i list1 list2 cst =
  if i >= size then (list1, list2, cst)
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
  val points_to_draw : t Abstract1.t -> (float * float) list
  (* val to_box : t Abstract1.t -> Environment.t -> Box.t Apron.Abstract1.t *)
  (* val to_oct : t Abstract1.t -> Environment.t -> Oct.t Apron.Abstract1.t *)
  (* val to_poly : t Abstract1.t -> Environment.t -> (Polka.strict Polka.t) Apron.Abstract1.t *)
 end
 
(** 
 * Module for the Box Abstract Domains for Constraint Programming.
 *)
module BoxCP  =
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
      let value = match Environment.typ_of_var env var with
	| Environment.INT -> scalar_plus_mpqf mid split_prec_mpqf 
	| _ -> mid
      in
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

    let points_to_draw box =
      let b = Abstract1.to_box man box in
      let i1 = Abstract1.(b.interval_array.(0))
      and i2 = Abstract1.(b.interval_array.(1)) in
      let open Interval in
      let x1,x2 = (scalar_to_float i1.inf, scalar_to_float i1.sup) in
      let y1,y2 = (scalar_to_float i2.inf, scalar_to_float i2.sup) in
      [x1,y1; x2,y1; x2,y2; x1,y2]

    let to_box box env = Abstract1.change_environment man box env false

    let to_oct box env = 
      let box' = Abstract1.change_environment man box env false in
      let bounds = Abstract1.to_box man box' in
      let (ivars, rvars) = Environment.vars env in
      let tvars = Array.append ivars rvars in
      let oct = Abstract1.of_box (Oct.manager_alloc ()) env tvars bounds.Abstract1.interval_array in
      oct
    
    let to_poly box env = 
      let box' = Abstract1.change_environment man box env false in
      let cons = Abstract1.to_lincons_array man box' in
      let poly = Abstract1.of_lincons_array (Polka.manager_alloc_strict ()) env cons in
      poly

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

    let sat_cons box cons = 
      tcons_for_all (Abstract1.sat_tcons man box) cons

    let points_to_draw box = []

    let to_box oct env = 
      let oct' = Abstract1.change_environment man oct env false in
      let bounds = Abstract1.to_box man oct' in
      let (ivars, rvars) = Environment.vars env in
      let tvars = Array.append ivars rvars in
      let box = Abstract1.of_box (Box.manager_alloc ()) env tvars bounds.Abstract1.interval_array in
      box

    let to_oct oct env = 
      Abstract1.change_environment man oct env false

    let to_poly oct env = 
      let oct' = Abstract1.change_environment man oct env false in
      let cons = Abstract1.to_lincons_array man oct' in
      let poly = Abstract1.of_lincons_array (Polka.manager_alloc_strict ()) env cons in
      poly

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

    let points_to_draw box = []

    let to_box oct env = 
      let oct' = Abstract1.change_environment man oct env false in
      let bounds = Abstract1.to_box man oct' in
      let (ivars, rvars) = Environment.vars env in
      let tvars = Array.append ivars rvars in
      let box = Abstract1.of_box (Box.manager_alloc ()) env tvars bounds.Abstract1.interval_array in
      box

    let to_oct oct env = 
      Abstract1.change_environment man oct env false

    let to_poly oct env = 
      let oct' = Abstract1.change_environment man oct env false in
      let cons = Abstract1.to_lincons_array man oct' in
      let poly = Abstract1.of_lincons_array (Polka.manager_alloc_strict ()) env cons in
      poly

  end

 
(** 
 * Module for the Octagon Abstract Domains for Constraint Programming.
 *)
module OctBoxCP =
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

    let points_to_draw oct = 
      let env = Abstract1.env oct in
      let x = Environment.var_of_dim env 0 
      and y = Environment.var_of_dim env 1 in
      let l' = Abstract1.to_lincons_array man oct in
      let manpolka = Polka.manager_alloc_strict() in
      let get_coord l = 
	(Linexpr1.get_coeff l x),(Linexpr1.get_coeff l y)
      in
      let pol = Abstract1.of_lincons_array manpolka env l' in
      let gen' = Abstract1.to_generator_array manpolka pol in
      let v = Array.init (Generator1.array_length gen')
	(fun i -> get_coord 
	  (Generator1.get_linexpr1 (Generator1.array_get gen' i)))
	     |> Array.to_list
      in 
    List.map (fun(a,b)-> (coeff_to_float a, coeff_to_float b)) v      

    let to_box oct env = 
      let oct' = Abstract1.change_environment man oct env false in
      let bounds = Abstract1.to_box man oct' in
      let (ivars, rvars) = Environment.vars env in
      let tvars = Array.append ivars rvars in
      let box = Abstract1.of_box (Box.manager_alloc ()) env tvars bounds.Abstract1.interval_array in
      box

    let to_oct oct env = 
      Abstract1.change_environment man oct env false

    let to_poly oct env = 
      let oct' = Abstract1.change_environment man oct env false in
      let cons = Abstract1.to_lincons_array man oct' in
      let poly = Abstract1.of_lincons_array (Polka.manager_alloc_strict ()) env cons in
      poly
  end

(** 
 * Module for the Polyhedron Abstract Domains for Constraint Programming.
 *)
module PolyCP = struct
  type t = Polka.strict Polka.t
  let man = Polka.manager_alloc_strict()
  let of_lincons_array env domains =
    let abs = Abstract1.of_lincons_array man env domains in
    abs
      
  let get_manager = man
    
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
      
  let points_to_draw pol =    
    let env = Abstract1.env pol in
    let x = Environment.var_of_dim env 0 
    and y = Environment.var_of_dim env 1 in
    let get_coord l = (Linexpr1.get_coeff l x),(Linexpr1.get_coeff l y) in
    let l' = Abstract1.to_lincons_array man pol in
    let pol = Abstract1.of_lincons_array man env l' in
    let gen' = Abstract1.to_generator_array man pol in
    let v = Array.init (Generator1.array_length gen')
      (fun i -> get_coord 
	(Generator1.get_linexpr1 (Generator1.array_get gen' i)))
	   |> Array.to_list
    in 
    List.map (fun(a,b)-> (coeff_to_float a, coeff_to_float b)) v

    let to_box poly env =
      let poly' = Abstract1.change_environment man poly env false in
      let bounds = Abstract1.to_box man poly' in
      let (ivars, rvars) = Environment.vars env in
      let tvars = Array.append ivars rvars in
      let box = Abstract1.of_box (Box.manager_alloc ()) env tvars bounds.Abstract1.interval_array in
      box

    let to_oct poly env =
      let poly' = Abstract1.change_environment man poly env false in
      let gens = Abstract1.to_generator_array man poly' in
      let dim = Environment.dimension env in
      let oct0 = Oct.of_generator_array (Oct.manager_alloc ()) dim.Dim.intd dim.Dim.reald gens.Generator1.generator0_array in
      let oct = {Abstract1.abstract0 = oct0; Abstract1.env = env} in
      oct

    let to_poly poly env =
      Abstract1.change_environment man poly env false
end
