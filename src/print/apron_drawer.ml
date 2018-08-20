open Apron
open Apron_domain
open Apron_utils
open Adcp_sig
open ADCP

module Make(AP:ADomain) = struct

  module A = Abstract1

  type t = AP.t A.t

  let man = AP.get_manager

  let polkaman = Polka.manager_alloc_strict ()

  let to_poly abs env =
    let abs' = A.change_environment man abs env false in
    A.to_lincons_array man abs' |> A.of_lincons_array polkaman env

  let print = Abstract1.print

  let bound abs v =
    let i = A.bound_variable man abs (Apron.Var.of_string v) in
    let open Interval in
    Apron_utils.(scalar_to_mpqf i.inf,scalar_to_mpqf i.sup)

  let is_empty = A.is_bottom man

  let to_abs (abs, consts) =
    abs

  let draw draw_f draw_dashed_f fillpol abs (v1,v2) col =
    let get_indexes env (x,y) = Environment.(
      dim_of_var env (Var.of_string x),
      dim_of_var env (Var.of_string y))
    in
    let vertices2d abs (v1,v2) =
      let env = A.env abs in
      let draw_pol pol =
        let i1,i2 = get_indexes env (v1,v2) in
        let x,y = Environment.(var_of_dim env i1,var_of_dim env i2) in
        let get_coord l = Linexpr1.(get_coeff l x,get_coeff l y) in
        let gen' = A.to_generator_array polkaman pol in
        let v = Array.init (Generator1.array_length gen')
	        (fun i -> get_coord
	          (Generator1.get_linexpr1 (Generator1.array_get gen' i)))
	         |> Array.to_list
        in
        List.map (fun(a,b)-> (coeff_to_float a, coeff_to_float b)) v
      in
      draw_pol (to_poly abs env)
    in
    let vert = vertices2d abs (v1,v2) in
    fillpol vert col;
    draw_f vert Graphics.black

  let draw2d =
    draw View.draw_poly 2 View.fill_poly

  let draw3d fmt abs_list (v1,v2,v3) =
    Format.printf "no 3d generation for apron domains for now\n%!"

  let print_latex fmt abs (v1,v2) col =
    Format.printf "no latex generation for apron domains for now\n%!"

end

module BoxDrawer = Make(struct
  type t = Box.t
  let get_manager =  Box.manager_alloc ()
end)

module OctDrawer = Make(struct
  type t = Oct.t
  let get_manager =  Oct.manager_alloc ()
end)

module PolyDrawer = Make(struct
  type t = Polka.strict Polka.t
  let get_manager = Polka.manager_alloc_strict()
end)

module type ApronReduction =
  sig
    module A : AbstractCP
    module B : ADomain
    val a_meet_b : A.t -> B.t Abstract1.t -> B.t Abstract1.t
    val man : B.t Manager.t
  end

module MakeVa(AP : ApronReduction) = struct

  module Abs = Abstract1

  let man = AP.man

  let polkaman = Polka.manager_alloc_strict ()

  let to_poly abs env =
    let abs' = Abs.change_environment man abs env false in
    Abs.to_lincons_array man abs' |> Abs.of_lincons_array polkaman env

  let print = Abs.print

  let bound (a, abs) v :(float * float) =
    let i = Abs.bound_variable man abs (Apron.Var.of_string v) in
    let open Interval in
    Apron_utils.(scalar_to_float i.inf,scalar_to_float i.sup)

  let draw draw_f draw_dashed_f fillpol (a, b) (v1,v2) col =
    let get_indexes env (x,y) = Environment.(
      dim_of_var env (Var.of_string x),
      dim_of_var env (Var.of_string y))
    in
    let vertices2d abs (v1,v2) =
      let env = Abs.env abs in
      let draw_pol pol =
        let i1,i2 = get_indexes env (v1,v2) in
        let x,y = Environment.(var_of_dim env i1,var_of_dim env i2) in
        let get_coord l = Linexpr1.(get_coeff l x,get_coeff l y) in
        let gen' = Abs.to_generator_array polkaman pol in
        let v = Array.init (Generator1.array_length gen')
	        (fun i -> get_coord
	          (Generator1.get_linexpr1 (Generator1.array_get gen' i)))
	         |> Array.to_list
        in
        List.map (fun(a,b)-> (coeff_to_float a, coeff_to_float b)) v
      in
      draw_pol (to_poly abs env)
    in
    let abs = AP.a_meet_b a b in
    let vert = vertices2d abs (v1,v2) in
    fillpol vert col;
    draw_f vert Graphics.black

  let draw2d =
    draw View.draw_poly 2 View.fill_poly

  let draw3d fmt abs_list (v1,v2,v3) =
    Format.printf "no 3d generation for apron domains for now\n%!"

  let print_latex fmt abs (v1,v2) col =
    Format.printf "no latex generation for apron domains for now\n%!"

end

module BoxAndPoly : ApronReduction =
  struct

    module A=BoxCP
    module B=struct
      type t = Polka.strict Polka.t
      let get_manager = Polka.manager_alloc_strict()
    end

    let a_meet_b box poly =
      let poly_env = Abstract1.env poly in
      let poly' = A.to_poly box poly_env in
      let poly' = Abstract1.meet B.get_manager poly poly' in
      poly'

    let man = B.get_manager

  end


module BoxNPolyDrawer = MakeVa(BoxAndPoly)
