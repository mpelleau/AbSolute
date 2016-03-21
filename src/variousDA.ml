(** 
 * This is an implementation of a CP solver using APRON.
 *)

open Apron
open Mpqf
open Format
open Utils
open ADCP

module type Reduction = 
  sig
    module A:AbstractCP
    module B:AbstractCP
    val a_meet_b : A.t Abstract1.t -> B.t Abstract1.t -> B.t Abstract1.t
    val b_meet_a : A.t Abstract1.t -> B.t Abstract1.t -> A.t Abstract1.t
    val reduced_product : A.t Abstract1.t -> B.t Abstract1.t -> A.t Abstract1.t * B.t Abstract1.t
  end

module BoxAndPoly : Reduction =
  struct

    module A=BoxCP
    module B=PolyCP

    let a_meet_b box poly =
      let poly_env = Abstract1.env poly in
      let poly' = BoxCP.to_poly box poly_env in
      Abstract1.meet PolyCP.get_manager poly poly'

    let b_meet_a box poly = 
      let box_env = Abstract1.env box in
      let box' = PolyCP.to_box poly box_env in
      Abstract1.meet BoxCP.get_manager box box'

    let reduced_product box poly =
      let new_poly = a_meet_b box poly in
      let new_box = b_meet_a box poly in
      (new_box, new_poly)
  end

module BoxAndOct : Reduction =
  struct

    module A=BoxCP
    module B=OctBoxCP

    let a_meet_b box oct =
      let oct_env = Abstract1.env oct in
      let oct' = BoxCP.to_oct box oct_env in
      Abstract1.meet OctBoxCP.get_manager oct oct'

    let b_meet_a box oct = 
      let box_env = Abstract1.env box in
      let box' = OctBoxCP.to_box oct box_env in
      Abstract1.meet BoxCP.get_manager box box'

    let reduced_product box oct =
      let new_oct = a_meet_b box oct in
      let new_box = b_meet_a box oct in
      (new_box, new_oct)
  end

module OctAndPoly : Reduction =
  struct

    module A = OctBoxCP
    module B = PolyCP

    let a_meet_b oct poly =
      let poly_env = Abstract1.env poly in
      let poly' = OctBoxCP.to_poly oct poly_env in
      Abstract1.meet PolyCP.get_manager poly poly'

    let b_meet_a oct poly = 
      let oct_env = Abstract1.env oct in
      let oct' = PolyCP.to_oct poly oct_env in
      Abstract1.meet OctBoxCP.get_manager oct oct'

    let reduced_product oct poly =
      let new_poly = a_meet_b oct poly in
      let new_oct = b_meet_a oct poly in
      (new_oct, new_poly)
  end

module Solve (Reduced : Reduction) =
  struct

    module TA = Apron_domain.SyntaxTranslator(Reduced.A)
    module TB = Apron_domain.SyntaxTranslator(Reduced.B)

    let man = Reduced.A.get_manager
    let man' = Reduced.B.get_manager

    let consistency abs tab =
      let abs' = Abstract1.meet_tcons_array man abs tab in
      (if Abstract1.is_bottom man abs' then `Empty
       else if tcons_for_all (Abstract1.sat_tcons man abs') tab then `Full
       else `Maybe), abs'

    let draw_a abs info col =
      if !Constant.visualization then
        Vue.draw (Reduced.A.points_to_draw abs) col info

    let draw_b abs info col =
      if !Constant.visualization then
        Vue.draw (Reduced.B.points_to_draw abs) col info

    let explore abs env tab abs' nb_steps nb_sol =
      let info = Vue.get_info (Reduced.A.points_to_draw abs) in
      draw_a abs info Graphics.yellow;
      let (abs, _) = Reduced.reduced_product abs abs' in
      let rec aux abs env nb_steps nb_sol =
        let cons, abs = consistency abs tab in
        match cons with
        | `Empty -> (nb_steps, nb_sol)
        | `Full ->
          draw_b (Reduced.a_meet_b abs abs') info Graphics.blue;
          (nb_steps, nb_sol+1)
        | `Maybe ->
          (match (Reduced.A.is_small abs !Constant.precision) with
          | true, _ ->
            draw_b (Reduced.a_meet_b abs abs') info Graphics.green;
            (nb_steps, nb_sol+1)
          | _, exprs when nb_sol <= !Constant.max_sol ->
            draw_a abs info Graphics.yellow;
            Reduced.A.split abs exprs |>
            List.fold_left (fun (a, b) c -> aux c env (a+1) b) (nb_steps, nb_sol)
          | _ -> (nb_steps, nb_sol)
          )
        in aux abs env nb_steps nb_sol

    let explore_breath_first abs env tab abs' nb_steps nb_sol =
      let info = Vue.get_info (Reduced.A.points_to_draw abs) in
      let nb_steps = ref nb_steps and nb_sol = ref nb_sol in
      let queue = Queue.create () in
      draw_a abs info Graphics.yellow;
      Queue.add abs queue;
      while Queue.is_empty queue |> not do
	let cons, abs = consistency (Queue.take queue) tab in
	match cons with
	| `Empty -> ()
	| `Full -> draw_b (Reduced.a_meet_b abs abs') info Graphics.blue; incr nb_sol
	| `Maybe  ->
	  (match (Reduced.A.is_small abs !Constant.precision) with
	  | true, _ -> draw_b (Reduced.a_meet_b abs abs') info Graphics.green; incr nb_sol
	  | _, exprs when !nb_sol < !Constant.max_sol ->
	    draw_a abs info Graphics.yellow;
            Reduced.A.split abs exprs |> List.iter (fun e -> incr nb_steps; Queue.add e queue)
	  | _ -> draw_b (Reduced.a_meet_b abs abs') info Graphics.green
	  )
      done;
      !nb_steps,!nb_sol

    let solving env domains cons cons' =
      let abs = Reduced.A.of_lincons_array env domains in
      printf "abs = %a@." Abstract1.print abs;
      let abs_aux = Reduced.B.of_lincons_array env domains in
      let abs' = Abstract1.meet_tcons_array man' abs_aux cons' in
      printf "abs' = %a@." Abstract1.print abs';
      let box = Abstract1.to_box man abs in
      let tab = box.Abstract1.interval_array in
      printf "box = %a@." (print_array Interval.print) tab;
      let box' = Abstract1.to_box man' abs' in
      let tab' = box'.Abstract1.interval_array in
      printf "box' = %a@." (print_array Interval.print) tab';
      let s = Manager.get_funopt man Manager.Funid_meet_tcons_array in
      let s' = {s with Manager.algorithm = 100} in
      Manager.set_funopt man Manager.Funid_meet_tcons_array s';
      if not (Abstract1.is_bottom man abs) then
        let (nb_steps, nb_sol) = explore abs env cons abs' 1 0 in
	match nb_sol with
	| 0 -> printf "No solutions - #created nodes: %d@." nb_steps
	| 1 -> printf "Unique solution - #created nodes: %d@." nb_steps
        | _ -> printf "#solutions: %d - #created nodes: %d@." nb_sol nb_steps
      else
        printf "No Solutions - #created nodes: 0@."

   let tcons1_print_array fmt tab = Tcons1.array_print fmt tab

    let solving solving_problem =
      let (env, domains, _, _, cons, cons') = TA.to_apron solving_problem in
      printf "cons = %a\ncons' = %a\n" tcons1_print_array cons tcons1_print_array cons';
      solving env domains cons' cons
  end

module BoxNOct = Solve(BoxAndOct)
module BoxNPoly = Solve(BoxAndPoly)
module OctNPoly = Solve(OctAndPoly)

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

(******************************************************************)
(**************************** Problems ****************************)
(******************************************************************)

let x = Var.of_string "x"
let y = Var.of_string "y"
let z = Var.of_string "z"
let x1 = Var.of_string "x1"
let x2 = Var.of_string "x2"
let x3 = Var.of_string "x3"
let x4 = Var.of_string "x4"
let x5 = Var.of_string "x5"
let x6 = Var.of_string "x6"
let x7 = Var.of_string "x7"
let x8 = Var.of_string "x8"
let x9 = Var.of_string "x9"

(* One solution: x=0, y=1, z=2 *)
let eqlin =
  let env = Environment.make [||] [|x; y; z|] in
  let domains = Parser.lincons1_of_lstring env ["x>=-1000"; "x<=1000"; "y>=-1000"; "y<=1000"; "z>=-1000"; "z<=1000"] in
  let list = [["3*x+5*y-3*z+1=0"];["x-2*y+z=0"];["2*x+4*y+7*z-18=0"]] in
  let cons = List.map (List.map (Parser.tcons1_of_string env)) list in
  (env, domains, cons)


(* Two solutions: y=0.618034, x=0.786151 ; y=0.618034, x=-0.786151 *)
let a =
  let env = Environment.make [||] [|x; y|] in
  let domains = Parser.lincons1_of_lstring env ["x>=-2"; "x<=2"; "y>=-2"; "y<=2"] in
  let list = [["x*x+y*y=1"];["x*x-y=0"]] in
  (*let list = [["2*x+y=0"];["x-2*y=0"]] in*)
  (*let list = [["(x-2)*(x-2)+(y-2)*(y-2)-1=0"];["(x-2)*(x-2)-(y-2)=0"]] in*)
  let cons = List.map (List.map (Parser.tcons1_of_string env)) list in
  (env, domains, cons)


(* No solutions! *)
let appolonius =
  let env = Environment.make [||] [|x1; x2; x3; x4; x5; x6; x7; x8|] in
  let domains = Parser.lincons1_of_lstring env ["x1>=-1000"; "x1<=1000"; "x2>=-1000"; "x2<=1000"; "x3>=-1000"; "x3<=1000"; "x4>=-1000"; "x4<=1000"; "x5>=-1000"; "x5<=1000"; "x6>=-1000"; "x6<=1000"; "x7>=-1000"; "x7<=1000"; "x8>=-1000"; "x8<=1000"] in
  let list = [["2*x1=2"]; ["2*x2=1"]; ["2*x3=2"]; ["2*x4=1"]; ["2*x5-x6=0"]; ["x5+2*x6=2"]; ["(x1-x7)*(x1-x7)+x8*x8-x7*x7-(x8-x2)*(x8-x2)=0"]; ["(x1-x7)*(x1-x7)+x8*x8-(x3-x7)*(x3-x7)-(x4-x8)*(x4-x8)=0"]; ["x1*x1+x2*x2-2*x1*x7+2*x2*x8=0"]; ["x1*x1-x3*x3*x3-x4*x4-2*x7*(x1-x3)+2*x4*x8=0"]] in
  let cons = List.map (List.map (Parser.tcons1_of_string env)) list in
  (env, domains, cons)


(* 8 solutions *)
let bellido =
  let env = Environment.make [||] [|x1; x2; x3; x4; x5; x6; x7; x8; x9|] in
  let domains = Parser.lincons1_of_lstring env ["x1>=-1000"; "x1<=1000"; "x2>=-1000"; "x2<=1000"; "x3>=-1000"; "x3<=1000"; "x4>=-1000"; "x4<=1000"; "x5>=-1000"; "x5<=1000"; "x6>=-1000"; "x6<=1000"; "x7>=-1000"; "x7<=1000"; "x8>=-1000"; "x8<=1000"; "x9>=-1000"; "x9<=1000"] in
  let list = [["(x1-6)*(x1-6)+x2*x2+x3*x3=104"]; ["x4*x4+(x5-6)*(x5-6)+x6*x6=104"]; ["x7*x7+(x8-12)*(x8-12)+(x9-6)*(x9-6)=80"]; ["x1*(x4-6)+x5*(x2-6)+x3*x6=52"]; ["x1*(x7-6)+x8*(x2-12)+x9*(x3-6)=-64"]; ["x4*x7+x8*(x5-12)+x9*(x6-6)-6*x5=-32"]; ["2*x2+2*x3-2*x6-x4-x5-x7-x9=-18"]; ["x1+x2+2*x3+2*x4+2*x6-2*x7+x8-x9=38"]; ["x1+x3+x5-x6+2*x7-2*x8-2*x4=-8"]] in
  let cons = List.map (List.map (Parser.tcons1_of_string env)) list in
  (env, domains, cons)


(* Two solutions: y=1, x=1 ; y=1, x=-1 *)
let entier =    
  let env = Environment.make [|x; y|] [||] in
  let domains = Parser.lincons1_of_lstring env ["x>=-3"; "x<=3"; "y>=-3"; "y<=3"] in
  (*let domains = Parser.lincons1_of_lstring env ["x>=-3"; "x<=2"; "y>=-3"; "y<=2"; "x+y>=-1"; "3y-6x<=-1"; "x+y<=2"; "6y-3x>=-2"] in*)
  let cons = List.map (List.map (Parser.tcons1_of_string env)) [["x*x-y=0"];["x*x+y-2=0"]] in
  (env, domains, cons)


(* Two solutions: y=1, x=1 ; y=1, x=-1 *)
let reel =    
  let env = Environment.make [||] [|x; y|] in
  let domains = Parser.lincons1_of_lstring env ["x>=-3"; "x<=3"; "y>=-3"; "y<=3"] in
  (*let domains = Parser.lincons1_of_lstring env ["x>=-3"; "x<=2"; "y>=-3"; "y<=2"; "x+y>=-1"; "3y-6x<=-1"; "x+y<=2"; "6y-3x>=-2"] in*)
  let cons = List.map (List.map (Parser.tcons1_of_string env)) [["x*x-y=0"];["x*x+y-2=0"]] in
  (env, domains, cons)


(* Two solutions: y=1, x=1 ; y=1, x=-1 *)
let mixte =    
  let env = Environment.make [|x|] [|y|] in
  let domains = Parser.lincons1_of_lstring env ["x>=-3"; "x<=3"; "y>=-3"; "y<=3"] in
  (*let domains = Parser.lincons1_of_lstring env ["x>=-3"; "x<=2"; "y>=-3"; "y<=2"; "x+y>=-1"; "3y-6x<=-1"; "x+y<=2"; "6y-3x>=-2"] in*)
  let cons = List.map (List.map (Parser.tcons1_of_string env)) [["x*x-y=0"];["x*x+y-2=0"]] in
  (env, domains, cons)
 *)
