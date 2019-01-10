open Apron
open Apron_domain
open Apron_utils

module Make(AP:ADomain) = struct

  module A = Abstract1

  type t = AP.t A.t

  let man = AP.get_manager

  module T = MAKE(AP)

  let polkaman = Polka.manager_alloc_strict ()

  let to_poly abs env =
    let abs' = A.change_environment man abs env false in
    A.to_lincons_array man abs' |> A.of_lincons_array polkaman env

  let print = Abstract1.print

  let bound abs v =
    let i = A.bound_variable man abs (Apron.Var.of_string v) in
    Apron.Interval.(scalar_to_rat i.inf,scalar_to_rat i.sup)

  let is_empty = A.is_bottom man

  let to_abs (abs, consts) =
    let csts_expr = Csp.csts_to_expr consts in
    let (csts_vars, _) = List.split consts in

    let new_vars = List.map (fun v -> (Csp.Real, v)) (csts_vars) in
    let a = List.fold_left (T.add_var) abs new_vars in

    List.fold_left (fun a c -> T.filter a c) a csts_expr

  let get_indexes env (x,y) = Environment.(
      dim_of_var env (Var.of_string x),
      dim_of_var env (Var.of_string y))

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


  let draw draw_f fillpol abs (v1,v2) col =
    let vert = vertices2d abs (v1,v2) in
    fillpol vert col;
    draw_f vert Graphics.black

  let draw2d =
    draw View.draw_poly View.fill_poly

  let draw3d fmt abs_list (v1,v2,v3) =
    Format.printf "no 3d generation for apron domains for now\n%!"

  let print_latex fmt abs (v1,v2) col =
    let l = vertices2d abs (v1, v2) in
    let l = View.graham_sort l in
    Latex.filldraw fmt l col

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
