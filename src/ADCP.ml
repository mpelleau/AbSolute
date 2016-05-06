open Apron
open Mpqf
open Format
open Utils
      
(** 
 * Module for Abstract Domains for Constraint Programming.
 * These are abstract domains with consistency, split and precision operators.
 *)
module type AbstractCP =
 sig
  type t
  type split
  val of_problem : Syntax.prog -> t
  val is_small : t -> float -> (bool * split list)
  val split : t -> split list -> t list
  val points_to_draw : t -> (string * string) option-> (float * float) list
  val is_bottom : t -> bool
  val sat_cons : t -> Syntax.bexpr -> bool
  val meet : t -> Syntax.bexpr -> t
  val print : Format.formatter -> t -> unit
  val forward_eval : t -> Syntax.expr -> (float * float)
 end
 
(** 
 * Module for the Box Abstract Domains for Constraint Programming.
 *)
module BoxCP =
  struct

    include Apron_domain.MAKE (struct
      type t = Box.t
      let get_manager =  Box.manager_alloc ()
    end)

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

    (* More efficient implementation of points to draw *)
    let points_to_draw box vars =
      let env = Abstract1.env box in
      let b = Abstract1.to_box man box in
      let i1,i2 = get_indexes env vars in
      let i1,i2 = 
	Abstract1.(b.interval_array.(i1)),
	Abstract1.(b.interval_array.(i2))
      in
      let open Interval in
      let x1,x2 = (scalar_to_float i1.inf, scalar_to_float i1.sup) in
      let y1,y2 = (scalar_to_float i2.inf, scalar_to_float i2.sup) in
      [x1,y1; x2,y1; x2,y2; x1,y2]

  end
 
(** 
 * Module for the Octagon Abstract Domains for Constraint Programming.
 *)
module OctMinMinCP =
  struct

    include Apron_domain.MAKE (struct
      type t = Oct.t
      let get_manager =  Oct.manager_alloc ()
    end)

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
        if j >= dim then (linexpr1, linexpr2, cst, max, min)
        else 
	  let var_j = Environment.var_of_dim env j in
          let (expr_max, expr_max', itv_max, diam_max, diam_min) = minmax_bij var_i var_j in
          if Scalar.cmp diam_min min < 0 then
            minmax_bi var_i (j+1) diam_min diam_max (mid_interval itv_max) expr_max expr_max'
          else
            minmax_bi var_i (j+1) min max cst linexpr1 linexpr2
      in
   
      (* Compute the min of all the min of the basis Bij and the corresponding max
       * min_{i \in \{1...n\}, j \in \{i+1...n\}} (min Bij) *)
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
      let (mmax, i_max, mmin, i_min) = minmax tab 1 (diam_interval tab.(0)) 0 (diam_interval tab.(0)) 0 in  
      let mmax' = Scalar.of_mpqf mmax in
      let expr1 = Linexpr1.make env in
      Linexpr1.set_list expr1 [(coeff1, Environment.var_of_dim env i_max)] None;
      let expr2 = Linexpr1.make env in
      Linexpr1.set_list expr2 [(coeff_1, Environment.var_of_dim env i_max)] None;
      let mmin' = Scalar.of_mpqf mmin in
      let (linexpr1, linexpr2, cst, max, min) = minmax_b 0 mmin' mmax' (mid_interval tab.(i_max)) expr1 expr2 in    
      let sca_cst = scalar_plus_mpqf cst split_prec_mpqf in
      Linexpr1.set_cst linexpr1 (Coeff.Scalar (Scalar.neg sca_cst));
      Linexpr1.set_cst linexpr2 (Coeff.Scalar sca_cst);      
      let max = scalar_to_float max in
      (max <= prec, [linexpr1; linexpr2])
  end

(** 
 * Module for the Octagon Abstract Domains for Constraint Programming.
 *)
module OctMinMaxCP =
  struct    

    include Apron_domain.MAKE (struct
      type t = Oct.t
      let get_manager =  Oct.manager_alloc ()
    end)

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
  end

 
(** 
 * Module for the Octagon Abstract Domains for Constraint Programming.
 *)
module OctBoxCP =
  struct

    include Apron_domain.MAKE (struct
      type t = Oct.t
      let get_manager =  Oct.manager_alloc ()
    end) 

    let is_small octad prec =
      let env = Abstract1.env octad in
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
  end

(** 
 * Module for the Polyhedron Abstract Domains for Constraint Programming.
 *)
module PolyCP = struct
  include Apron_domain.MAKE (struct
    type t = Polka.strict Polka.t
    let get_manager = Polka.manager_alloc_strict()
  end)
      
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
end
