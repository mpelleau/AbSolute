open Abstract_box.BoxStrict

type t = Abstract_box.BoxStrict.t

let print = Abstract_box.BoxStrict.print

let bound abs v = find v abs |> fst |> I.to_rational_range

let is_empty = Abstract_box.BoxStrict.is_empty

let to_abs (abs, consts) =
  let csts_expr = Csp.csts_to_expr consts in
  let (csts_vars, _) = List.split consts in

  let new_vars = List.map (fun v -> (Csp.REAL, v)) (csts_vars) in
  let a = List.fold_left (Abstract_box.BoxStrict.add_var) abs new_vars in

  List.fold_left (fun a c -> Abstract_box.BoxStrict.filter a c) a csts_expr

let draw draw_f draw_dashed_f fillpol abs (v1,v2) col =
  let (xl,xu) = bound abs v1 and (yl,yu) = bound abs v2 in
  fillpol [(xl,yl);(xl,yu);(xu,yu);(xu,yl)] col;
  let ((xl,xu) as i1) : I.t = find v1 abs |> fst
  and ((yl,yu) as i2) : I.t = find v2 abs |> fst in
  let draw_seg vert a (b,c) =
    let open Newitv.Test in
    let draw_f,value =
      match a with
      | Strict,x -> draw_f ,x
      | Large,x  -> draw_dashed_f,x
    in
    (if vert then draw_f (value,b) (value,c) Graphics.black
     else draw_f (b,value) (c,value) Graphics.black)
  in
  draw_seg true xl (I.to_float_range i2);
  draw_seg true xu (I.to_float_range i2);
  draw_seg false yl (I.to_float_range i1);
  draw_seg false yu (I.to_float_range i1)

let print_latex fmt =
  Latex.(draw (drawseg fmt) (draw_dashed_seg fmt) (fillpol fmt))

let draw2d = View.(draw draw_seg draw_dashed_seg fill_poly_mpqf)

let draw3d fmt abs_list (v1,v2,v3) =
  let make_cube (a,b) (c,d) (e,f) = ((a,c,e), b-.a, d-.c, f-.e) in
  let cube e = make_cube (View.to_float(bound e v1)) (View.to_float(bound e v2)) (View.to_float(bound e v3)) in
  let cubes = List.rev_map (fun e -> cube e) abs_list in
  let o = Objgen.build_cube_list cubes in
  Format.fprintf fmt "%a" Objgen.print o
