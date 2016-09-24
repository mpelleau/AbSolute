open Itv_sig
open Abstract_box.BoxF

type t = Abstract_box.BoxF.t

let print = Abstract_box.BoxF.print

let bound abs v = find v abs |> fst |> I.to_float_range

let draw draw_f fillpol abs (v1,v2) col =
  let (xl,xu) = bound abs v1 and (yl,yu) = bound abs v2 in
  fillpol [(xl,yl);(xl,yu);(xu,yu);(xu,yl)] col;
  let ((xl,xu) as i1) : I.t = find v1 abs |> fst
  and ((yl,yu) as i2) : I.t = find v2 abs |> fst in
  let draw_seg vert value (b,c) =
    if vert then draw_f (value,b) (value,c) Graphics.black
    else draw_f (b,value) (c,value) Graphics.black
  in
  draw_seg true xl (I.to_float_range i2);
  draw_seg true xu (I.to_float_range i2);
  draw_seg false yl (I.to_float_range i1);
  draw_seg false yu (I.to_float_range i1)

let draw2d = View.(draw draw_seg fill_poly)

let print_latex fmt = Latex.(draw (drawseg fmt) (fillpol fmt))

let draw3d abs_list (v1,v2,v3) = failwith "niy"
