let xmin = ref 0.
and xmax = ref 0.
and ymin = ref 0.
and ymax = ref 0.

let sx = ref 0.
let sy = ref 0.

let fff = Format.fprintf

let escape name = Str.(global_replace (regexp "_")) "" name

let red_comp rgb = (rgb / (0xFFFF) |> float) /. 255.
let green_comp rgb = ((rgb / 255) mod 256 |> float) /. 255.
let blue_comp rgb = (rgb mod 256 |> float) /. 255.

let rgb_to_latex_col col =
  if col = Graphics.black then "black"
  else if col = Graphics.green then "fill = unsure"
  else if col = Graphics.blue then "blue"
  else "fill = sure"

let scale_x x =
  !sx *. (x -. !xmin) /. (!xmax -. !xmin)

let scale_y y =
  !sy *. (y -. !ymin) /. (!ymax -. !ymin)

let drawseg fmt (x1,y1) (x2,y2) col =
  let c = rgb_to_latex_col col in
  fff fmt "    \\draw [%s] (%f,%f) -- (%f,%f);\n" c x1 y1 x2 y2

let draw_dashed_seg fmt (x1,y1) (x2,y2) col =
  let c = rgb_to_latex_col col in
  fff fmt "    \\draw [%s, dashed] (%f,%f) -- (%f,%f);\n" c x1 y1 x2 y2

let fillpol fmt l col =
  let c = rgb_to_latex_col col in
  let l = View.l_to_float l in
	fff fmt "    \\fill [%s] " c;
	List.iter (fun (x,y) -> fff fmt "(%f, %f) -- " x y) l;
  fff fmt "cycle;@."

let filldraw fmt l col =
  let c = rgb_to_latex_col col in
  fff fmt "    \\filldraw [%s] " c;
  List.iter (fun (x,y) -> fff fmt "(%f, %f) -- " x y) l;
  fff fmt "cycle;@."

let filldrawbox fmt (xl, yl) (xu, yu) col =
  let c = rgb_to_latex_col col in
  let xl = Bound_rat.to_float_up xl in
  let xu = Bound_rat.to_float_up xu in
  let yl = Bound_rat.to_float_up yl in
  let yu = Bound_rat.to_float_up yu in
  fff fmt "    \\%s [%s] (%f, %f) rectangle (%f, %f);@."
    (if col = Graphics.green then "draw" else "filldraw") c xl yl xu yu

let init (a,b) (c,d) =
  xmin := a;
  xmax := b;
  ymin := c;
  ymax := d

let create x y =
  sx := x;
  sy := y
