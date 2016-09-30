open Apron
open Apron_domain
open Apron_utils

module Make(AP:ADomain) = struct

  module A = Abstract1

  type t = AP.t A.t

  let man = AP.get_manager

  let to_poly abs env =
    let abs' = A.change_environment man abs env false in
    A.to_lincons_array man abs' |>
        A.of_lincons_array (Polka.manager_alloc_strict ()) env

  let print = Abstract1.print

  let bound abs v :(float * float) =
    let i = A.bound_variable man abs (Apron.Var.of_string v) in
    let open Interval in
    Apron_utils.(scalar_to_float i.inf,scalar_to_float i.sup)

  let draw2d abs (v1,v2) col =
    (* given two variables to draw, and an environnement,
       returns the two variables value in the environment. *)
    let get_indexes env (x,y) =
	    (Environment.dim_of_var env (Var.of_string x)),
	    (Environment.dim_of_var env (Var.of_string y))
    in
    let vertices2d abs (v1,v2) =
      let env = A.env abs in
      let draw_pol pol =
        let i1,i2 = get_indexes env (v1,v2) in
        let x = Environment.var_of_dim env i1
        and y = Environment.var_of_dim env i2 in
        let get_coord l = (Linexpr1.get_coeff l x),(Linexpr1.get_coeff l y) in
        let gen' = A.to_generator_array (Polka.manager_alloc_strict ()) pol in
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
    View.fill_poly vert col;
    View.draw_poly vert Graphics.black

  let draw3d fmt abs_list (v1,v2,v3) = failwith "niy"

  let print_latex fmt abs (v1,v2) col = failwith ""

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
