open Graphics

let padding = 15.
let x_min = ref 0.
and x_max = ref 0.
and y_min = ref 0.
and y_max = ref 0.
and sx = ref 0.
and sy = ref 0.

let to_float (a, b) =
  (Mpqf.to_float a, Mpqf.to_float b)

let l_to_float l = List.map (to_float) l

(* graham sort *)
let signF f = if f < 0. then -1. else 1.

let bottom_left l =
  let rec aux (best_x,best_y) l =
    match l with
    | [] -> best_x,best_y
    | (x,y)::b ->
      if best_y > y || best_y = y && x < best_x then aux (x,y) b
      else aux (best_x,best_y) b
  in aux (List.hd l) (List.tl l)

let angle (ax,ay) (bx,by) =
  let (ax,ay) = (bx -. ax), (by -. ay)
  and (bx,by) = (1.,0.) in
  let na = sqrt (ax *. ax +. ay *. ay)
  and nb = sqrt (bx *. bx +. by *. by) in
  let c = (ax *. bx +. ay *. by)/.(na*.nb)
  and s = (ax *. by -. ay *. bx) in
  (signF s)*.(acos c)

let graham_sort l =
  let p = bottom_left l in
  let comp p1 p2 =
    if p1 = p then 1
    else if p2 = p then -1
    else if angle p p1 < angle p p2 then 1
    else if angle p p1 = angle p p2 then 0
    else -1
  in List.sort comp l

let projection (a,b) (c,d) n =
  let perc (x,y) r = x +. (r *. (y-.x))
  and to_perc (x,y) r =
    if x < 0. then (r-.x) /. (y-.x)
    else (r-.x) /. (y-.x)
  in
  if b = a then c else perc (c,d) (to_perc (a,b) n)

let to_coord (min_x,max_x) (min_y,max_y) (a,b) =
  let a = projection (min_x,max_x) (padding, (!sx-.padding)) a
  and b = projection (min_y,max_y) (padding, (!sy-. (2. *. padding))) b
  in (int_of_float a, int_of_float b)

let normalize p =
  to_coord (!x_min,!x_max) (!y_min,!y_max) p

let fill_circle a b r =
  let a, b = to_float (a, b) in
  let a,b = normalize (a,b) in
  fill_circle a b r

let fill_poly_mpqf l col =
  set_color col;
  let l = l_to_float l in
  let l = graham_sort l in
  let n = List.length l in
  let arr = Array.make n (0,0) in
  List.iteri (fun i (x,y) -> arr.(i) <- normalize (x,y)) l;
  fill_poly arr

let fill_poly l col =
  set_color col;
  let l = graham_sort l in
  let n = List.length l in
  let arr = Array.make n (0,0) in
  List.iteri (fun i (x,y) -> arr.(i) <- normalize (x,y)) l;
  fill_poly arr

let draw_poly l col =
  set_color col;
  let l = graham_sort l in
  let n = List.length l in
  let arr = Array.make n (0,0) in
  List.iteri (fun i (x,y) -> arr.(i) <- normalize (x,y)) l;
  draw_poly arr

let draw_seg_mpqf p1 p2 col =
  set_color col;
  let p1 = to_float p1 and
      p2 = to_float p2 in
  let x1,y1 = normalize p1
  and x2,y2 = normalize p2
  in draw_segments [|(x1, y1, x2, y2)|]

let draw_seg p1 p2 col =
  set_color col;
  let x1,y1 = normalize p1
  and x2,y2 = normalize p2
  in draw_segments [|(x1, y1, x2, y2)|]

let draw_dashed_seg ((x1,y1) as p1) (x2,y2) col =
  let man_dist = abs_float (x2-.x1) +. abs_float (y2-.y1) in
  let step = 1. in
  let move (a,b) =
    (a+.((x2-.x1)*.step/.man_dist)),
    (b+.((y2-.y1)*.step/.man_dist))
  in
  let rec aux (cur_x,cur_y) =
    if abs_float (x2-.cur_x)+.abs_float (y2-.cur_y) < 2. *. step then
      draw_seg (cur_x,cur_y) (x2,y2) col
    else begin
      let next = move (cur_x,cur_y) in
      draw_seg (cur_x,cur_y) next col;
      aux (move next)
    end
  in aux p1

(****************)
(* AXIS DRAWING *)
(****************)

let draw_line_x x col = draw_seg (x, 0.) (x, size_y() |> float_of_int) col

let draw_line_y y col = draw_seg (0., y) ((size_x() |> float_of_int
), y) col

let draw_string x y str col =
  set_color col;
  moveto (int_of_float x) (int_of_float y);
  draw_string str

let set_font_size () : unit =
  Graphics.set_font "*--0-0-*iso8859-*"

let init (xl,xu) (yl,yu) =
  set_font_size ();
  x_min := xl;
  x_max := xu;
  y_min := yl;
  y_max := yu

let draw_end v1 v2 =
  let lg = rgb 200 200 200
  and sx = size_x() |> float_of_int
  and sy = size_y() |> float_of_int in
  let x_max = Format.asprintf "%.2f" !x_max in
  let y_max = Format.asprintf "%.2f" !y_max in
  let x_min = Format.asprintf "%.2f" !x_min in
  let y_min = Format.asprintf "%.2f" !y_min in
  (* drawing x axis *)
  draw_line_x padding lg;
  draw_string padding (padding/.2. -. 5.) x_min black;
  draw_line_x (sx-.padding) lg;
  draw_string (sx-. 2.*.padding) (padding/.2. -. 5.) x_max black;
  draw_string (sx-. 2.*.padding) (padding/.2. +. 15.) v1 black;
  (* drawing y axis *)
  draw_line_y padding lg;
  draw_string (padding/.2.-.5.) (padding+.5.) y_min black;
  draw_line_y (sy -. padding) lg;
  draw_string (padding/.2.-.5.) (sy-.2.*.padding-.5.) y_max black;
  draw_string (padding/.2.-.5.) (sy-.2.*.padding+.15.) v2 black

(* dummy main loop *)
let loop state =
  loop_at_exit [] (fun _ -> ())

(* window creation and initialization *)
let create_window width height =
  Format.sprintf " %ix%i" width height |> open_graph;
  sx := size_x() |> float;
  sy := size_y() - 10 |> float;
  set_window_title "AbSolute";
  loop ()
