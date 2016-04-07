open Graphics
open Format
open Apron
open Utils

(* graham sort *)
let signF f = if f < 0. then -1. else 1.
    
let bas_gauche l = 
  let rec aux (best_x,best_y) l = 
    match l with
    | [] -> best_x,best_y
    | (x,y)::b -> 
      if best_y > y || best_y = y && x < best_x then
	aux (x,y) b
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
  let p = bas_gauche l in
  let comp p1 p2 =
    if p1 = p then 1
    else if p2 = p then -1
    else if angle p p1 < angle p p2 then 1 
    else if angle p p1 = angle p p2 then 0
    else -1
  in
  List.sort comp l

(* Useful fonctions *)
let red rgb = rgb / (0xFFFF)
let green rgb = (rgb / 255) mod 255
let blue rgb = rgb mod 255

let getcomp c = 
  let r = red c |> float
  and g = green c |> float
  and b = blue c |> float in
  (r,g,b)

let draw draw_rect (x,y) w h col alpha =
  for i=x to x+w do
    for j=y to y+h do
      let (o_r,o_g,o_b) = point_color i j |> getcomp
      and (n_r,n_g,n_b) = getcomp col in
      let r = (alpha *. n_r) +. (1. -. alpha) *. o_r |> int_of_float 
      and g = (alpha *. n_g) +. (1. -. alpha) *. o_g |> int_of_float 
      and b = (alpha *. n_b) +. (1. -. alpha) *. o_b |> int_of_float in
      set_color (rgb r g b);
      plot i j
    done
  done
 
(* returns the bounds of a generator list *)
let get_info l =
  match l with
  | (x,y)::tl ->
    let (min_x,max_x) = (x,x) and (min_y,max_y) = (y,y) in
    List.fold_left (fun ((min_x,max_x),(min_y,max_y)) (x,y) ->
      let min_x = min min_x x and max_x = max max_x x
      and min_y = min min_y y and max_y = max max_y y in
      (min_x,max_x),(min_y,max_y)
    ) ((min_x,max_x),(min_y,max_y)) l
  | _ -> assert false
  
let projection (a,b) (c,d) n =
  let r = (d-.c)/.(b-.a) in
  (n-.a) *. r +. c

let to_coord (min_x,max_x) (min_y,max_y) (a,b) =
  let padding = 40. and s_x = size_x() |> float and s_y = size_y() |> float in
  let a = projection (min_x,max_x) (padding, (s_x-.padding)) a
  and b = projection (min_y,max_y) (padding, (s_y-.padding)) b
  in (int_of_float a, int_of_float b)
  
let padding = 50.

let to_coord (min_x,max_x) (min_y,max_y) (a,b) =
  let s_x = size_x() |> float and s_y = size_y() |> float in
  let a = projection (min_x,max_x) (padding, (s_x-.padding)) a
  and b = projection (min_y,max_y) (padding, (s_y-.padding)) b
  in (int_of_float a, int_of_float b)

let draw_line_x x col =
  set_color col;
  draw_segments [|(x, 0, x, size_y())|]

let draw_line_y y col =
  set_color col;
  draw_segments [|(0, y, size_x(), y)|]

let draw_string x y str col =
  set_color col;
  moveto x y; draw_string str

let draw_end ((x_min,x_max),(y_min,y_max)) =
  let f_coord = to_coord (x_min,x_max) (y_min,y_max) in
  let p = int_of_float padding and lg = rgb 200 200 200
  and sx = size_x() and sy = size_y() in
  let nb = 10 in
  let rangex = x_max -. x_min and rangey = y_max -. y_min in
  let stepx = rangex /. (float nb) and stepy = rangey /. (float nb) in
  set_color (rgb 160 240 160);
  for i = 0 to nb do
    let x = x_min +. (float i) *. stepx
    and y = y_min +. (float i) *. stepy in
    let a,_ = f_coord (x,0.)
    and c,_=f_coord (x, float sy) in
    draw_segments [|(a,0,c,sy)|];
    let _,b = f_coord (0.,y) 
    and _,d = f_coord (float sx, y) in
    draw_segments [|(0,b,sx,d)|]
  done;
  draw_line_x p lg;
  draw_string p (p/2 - 5) (string_of_float x_min) black;
  draw_line_x (sx-p) lg;
  draw_string (sx-p) (p/2 - 5) (string_of_float x_max) black;
  draw_line_y p lg;
  draw_string (p/2 -5 ) (p+5) (string_of_float y_min) black;
  draw_line_y (sy - p) lg;
  draw_string (p/2-5) (sy-p+5) (string_of_float y_max) black;
  let (a,b) = f_coord (0.,0.) in
  draw_line_y a (rgb 255 160 30);
  draw_line_x b (rgb 255 160 30)

let draw dom col ((x_min,x_max),(y_min,y_max)) =
  set_color col;
  let ((f1,f2) as a) = List.hd dom in
  if List.for_all (( = ) a) dom then
    let (a,b) = to_coord (x_min,x_max) (y_min,y_max) (f1,f2) in
    fill_circle a b 2
  else begin
    let l = graham_sort dom in
    let f_coord = to_coord (x_min,x_max) (y_min,y_max) in
    let l = List.rev_map f_coord l in
    fill_poly (Array.of_list l);
    set_color black;
    draw_poly (Array.of_list l)
  end

let clear dom ((x_min,x_max),(y_min,y_max)) =
  set_color (white);
  let l = List.rev_map (to_coord (x_min,x_max) (y_min,y_max)) dom in
  fill_poly (Array.of_list l)

let loop state = 
  loop_at_exit [] (fun _ -> ())

let create_window width height =
  Format.sprintf " %ix%i" width height |> open_graph;
  set_window_title "AbSolute";
  loop ()
