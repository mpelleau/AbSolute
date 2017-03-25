let fff = Format.fprintf

let escape name = Str.(global_replace (regexp "_")) "" name

let red_comp rgb = (rgb / (0xFFFF) |> float) /. 255.
let green_comp rgb = ((rgb / 255) mod 256 |> float) /. 255.
let blue_comp rgb = (rgb mod 256 |> float) /. 255.

let rgb_to_latex_col col =
  if col = Graphics.black then "black"
  else if col = Graphics.green then "green"
  else if col = Graphics. blue then "blue"
  else
    let r = red_comp col
    and g = green_comp col
    and b = blue_comp col in
    Format.sprintf "{rgb:red,%f;green,%f;blue,%f}" r g b

let drawseg fmt (x1,y1) (x2,y2) col =
  let c = rgb_to_latex_col col in
  fff fmt "\\draw[%s] (%f,%f) -- (%f,%f);\n" c x1 y1 x2 y2

let draw_dashed_seg fmt (x1,y1) (x2,y2) col =
  let c = rgb_to_latex_col col in
  fff fmt "\\draw[%s, dashed] (%f,%f) -- (%f,%f);\n" c x1 y1 x2 y2

let fillpol fmt l col =
  let c = rgb_to_latex_col col in
	fff fmt "\\fill[%s] " c;
	List.iter (fun (x,y) -> fff fmt "(%f, %f) -- " x y) l;
  fff fmt "cycle;@."

let filldraw fmt l col =
  let c = rgb_to_latex_col col in
  fff fmt "\\filldraw[%s, fill opacity = 0.3] " c;
  List.iter (fun (x,y) -> fff fmt "(%f, %f) -- " x y) l;
  fff fmt "cycle;@."

let filldrawbox fmt (xl, yl) (xu, yu) col =
  let c = rgb_to_latex_col col in
  fff fmt "\\filldraw[%s, fill opacity = 0.3] (%f, %f) rectangle (%f, %f);@." c xl yl xu yu
