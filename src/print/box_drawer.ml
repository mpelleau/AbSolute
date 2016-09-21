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
    if vert then draw_f (value,b) (value,c)
    else draw_f (b,value) (c,value)
  in
  draw_seg true xl (I.to_float_range i2);
  draw_seg true xu (I.to_float_range i2);
  draw_seg false yl (I.to_float_range i1);
  draw_seg false yu (I.to_float_range i1)


let draw2d =
  draw
    (fun (a,b) (c,d) -> View.draw_seg (a,b) (c,d) Graphics.black)
    View.fill_poly

let print_latex fmt =
  let drawseg =  fun (x1,y1) (x2,y2) ->
    Format.fprintf fmt "\\draw[black] (%f,%f) -- (%f,%f);\n" x1 y1 x2 y2
  and fillpol = fun l col ->
    let col = if col = Graphics.green then "green" else "blue" in
	  Format.fprintf fmt "\\filldraw[%s] " col;
	  List.iter (fun (x,y) ->
      Format.fprintf fmt "(%f, %f) -- " x y
    ) l;
    Format.fprintf fmt "cycle;@."
  in
  draw drawseg fillpol

let draw3d abs_list (v1,v2,v3) = failwith "niy"
