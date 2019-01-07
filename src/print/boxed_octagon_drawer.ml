open Boxed_octagon

let fail n = Pervasives.failwith (": Boxed_octagon_drawer." ^ n ^ ": unimplemented")

module A = BoxedOctagonF
type t = A.t
let is_empty = A.is_empty
let print = A.print
let bound = A.var_bounds

let to_abs (o, consts) = A.join_vars o consts

let draw2d : t -> (Csp.var * Csp.var) -> Graphics.color -> unit
 = fun o vars col ->
  let points = A.shape2d o vars in
  (* Draw the segments *)
  View.fill_poly points col;
  View.draw_poly points Graphics.black

let print_latex : Format.formatter -> t -> (Csp.var * Csp.var) -> Graphics.color -> unit
    = fun _ _ _ _ -> fail "print_latex"

let draw3d : Format.formatter -> t list -> (Csp.var * Csp.var * Csp.var) -> unit
    = fun _ _ _ -> fail "draw3d"
