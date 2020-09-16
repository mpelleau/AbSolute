
module Make(A:Adcp_sig.AbstractCP) = struct
  type t = A.t
  let print = A.print

  let bound = A.var_bounds

  let is_empty = A.is_empty

  let to_abs (abs, consts) =
    let csts_expr = Csp_helper.csts_to_expr consts in
    let (csts_vars, _) = List.split consts in

    let new_vars = List.map (fun v -> (Csp.Real, v)) (csts_vars) in
    let a = List.fold_left (A.add_var) abs new_vars in

    List.fold_left (fun a c -> A.filter a c) a csts_expr


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

  let draw2d =
    draw View.draw_seg_mpqf View.fill_poly_mpqf

  let print_latex fmt = Latex.(fill (filldrawbox fmt))

  let draw3d fmt abs_list (v1,v2,v3) =
    let make_cube (a,b) (c,d) (e,f) =
      ((a,c,e),(b,c,e),
       (b,d,e),(a,d,e),
       (a,c,f),(b,c,f),
       (b,d,f),(a,d,f))
    in
    let cube e =
      make_cube
        (View.to_float (bound e v1))
        (View.to_float (bound e v2))
        (View.to_float (bound e v3)) in
    let cubes = List.rev_map cube abs_list in
    Objgen.print_to_file fmt cubes
end
