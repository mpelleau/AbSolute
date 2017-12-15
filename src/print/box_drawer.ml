
module Make(A:Adcp_sig.AbstractCP) = struct
  type t = A.t
  let print = A.print

  let bound = A.var_bounds

  let draw draw_f fillpol abs (v1,v2) col =
    let ((xl,xu) as i1) = bound abs v1 and ((yl,yu) as i2) = bound abs v2 in
    fillpol [(xl,yl);(xl,yu);(xu,yu);(xu,yl)] col;
    let draw_seg vert value (b,c) =
      if vert then draw_f (value,b) (value,c) Graphics.black
      else draw_f (b,value) (c,value) Graphics.black
    in
    draw_seg true xl i2;
    draw_seg true xu i2;
    draw_seg false yl i1;
    draw_seg false yu i1

  let fill fillbox abs (v1,v2) col =
    let (xl,xu) = bound abs v1 and (yl,yu) = bound abs v2 in
    fillbox (xl,yl) (xu,yu) col

  let draw2d = View.(draw draw_seg fill_poly)

  let print_latex fmt = Latex.(fill (filldrawbox fmt))

  let draw3d fmt abs_list (v1,v2,v3) =
    let make_cube (a,b) (c,d) (e,f) = ((a,c,e), b-.a, d-.c, f-.e) in
    let cube e = make_cube (bound e v1) (bound e v2) (bound e v3) in
    let cubes = List.rev_map (fun e -> cube e) abs_list in
    let o = Objgen.build_cube_list cubes in
    Format.fprintf fmt "%a" Objgen.print o
end
