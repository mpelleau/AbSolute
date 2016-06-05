(* Reduced product of domains *)

open Apron
open Format
open Utils
open Adcp_sig
open ADCP

module type Reduction = 
  sig
    module A : AbstractCP
    module B : AbstractCP
    val a_meet_b : A.t -> B.t -> B.t 
    val b_meet_a : A.t -> B.t -> A.t 
  end

module BoxAndPoly : Reduction =
  struct

    module A=BoxCP
    module B=PolyCP

    let a_meet_b box poly =
      let poly_env = Abstract1.env poly in
      let poly' = BoxCP.to_poly box poly_env in
      Abstract1.meet PolyCP.man poly poly'

    let b_meet_a box poly = 
      let box_env = Abstract1.env box in
      let box' = PolyCP.to_box poly box_env in
      Abstract1.meet BoxCP.man box box'

  end

module BoxAndOct : Reduction =
  struct

    module A=BoxCP
    module B=OctBoxCP

    let a_meet_b box oct =
      let oct_env = Abstract1.env oct in
      let oct' = BoxCP.to_oct box oct_env in
      Abstract1.meet OctBoxCP.man oct oct'

    let b_meet_a box oct = 
      let box_env = Abstract1.env box in
      let box' = OctBoxCP.to_box oct box_env in
      Abstract1.meet BoxCP.man box box'

  end

module OctAndPoly : Reduction =
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

module VariousDomain_MS (Reduced : Reduction) : AbstractCP =
  struct

    include Reduced

    type t = A.t * B.t

    let reduced_product a b = 
      let new_a = b_meet_a a b in
      let new_b = a_meet_b a b in
      (new_a, new_b)

    let of_problem p =
      let open Syntax in
        let abs = A.of_problem p
        and tmp =  B.of_problem p in
        (* let cons_b = List.filter Syntax.is_cons_linear p.constraints in *)
        (* let abs' = List.fold_left B.filter tmp cons_b in *)
        reduced_product abs tmp
    
    let is_small ((abs, abs'):t) = A.is_small abs

    let prune (a,a') (b,b') = 
      let la,ua = A.prune a b
      and lb,ub = B.prune a' b' in
      [],(a,a')

    let split ((abs, abs'):t) = 
      let split_a = A.split abs in
      List.map (fun x -> (x, abs')) split_a

    let points_to_draw ((abs, abs'):t) vars =
      let abs_to_draw = a_meet_b abs abs' in
      B.points_to_draw abs_to_draw vars

    let is_bottom ((abs, _):t) = A.is_bottom abs

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
      B.print fmt abs'

  end

module BoxNOct = VariousDomain_MS(BoxAndOct)
module BoxNPoly = VariousDomain_MS(BoxAndPoly)
module OctNPoly = VariousDomain_MS(OctAndPoly)


(*
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


let box_meet_poly manbox box manpoly poly =
  let box_env = Abstract1.env box in
  let poly_env = Abstract1.env poly in
  let box2poly = box_to_poly manbox box manpoly poly_env in
  Abstract1.meet_with manpoly poly box2poly;
  let poly2box = poly_to_box manpoly poly manbox box_env in
  Abstract1.meet_with manbox box poly2box;


let oct_meet_poly manoct oct manpoly poly =
  let oct_env = Abstract1.env oct in
  let poly_env = Abstract1.env poly in
  let oct2poly = oct_to_poly manoct oct manpoly poly_env in
  Abstract1.meet_with manpoly poly oct2poly;
  let poly2oct = poly_to_oct manpoly poly manoct oct_env in
  Abstract1.meet_with manoct oct poly2oct;


let abs_meet_abs man abs abs' =
  let env = Abstract1.env abs in
  let env' = Abstract1.env abs' in
  let abs_tmp1 = Abstract1.change_environment man abs env' false in
  Abstract1.meet_with man abs' abs_tmp1;
  let abs_tmp2 = Abstract1.change_environment man abs' env false in
  Abstract1.meet_with man abs abs_tmp2;

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
 *)
