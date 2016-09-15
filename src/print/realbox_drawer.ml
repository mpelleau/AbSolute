open Itv_sig
open Abstract_box.BoxF

type t = Abstract_box.BoxF.t

let print = Abstract_box.BoxF.print

let bound abs v = find v abs |> fst |> I.to_float_range

let draw2d abs (v1,v2) col =
  let (xl,xu) = bound abs v1 and (yl,yu) = bound abs v2 in
  View.fill_poly [(xl,yl);(xl,yu);(xu,yu);(xu,yl)] col;
  let ((xl,xu) as i1) : I.t = find v1 abs |> fst
  and ((yl,yu) as i2) : I.t = find v2 abs |> fst in
  let draw_seg vert a (b,c) =
    let open Newitv.Test in
    let draw_f,value=
      match a with
      | Strict,x -> View.draw_dashed_seg,x
      | Large,x  -> View.draw_seg,x
    in
    (if vert then draw_f (value,b) (value,c)
     else draw_f (b,value) (c,value)) Graphics.black
  in
  draw_seg true xl (I.to_float_range i2);
  draw_seg true xu (I.to_float_range i2);
  draw_seg false yl (I.to_float_range i1);
  draw_seg false yu (I.to_float_range i1)

let draw3d abs_list (v1,v2,v3) = failwith "niy"
