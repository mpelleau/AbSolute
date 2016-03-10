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
    and (bx,by) = (1.,0.)
    in 
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

let to_coord (min_x,max_x) (min_y,max_y) (x,y) =
  let padding = 150. and s_x = size_x() |> float and s_y = size_y() |> float in
  let x = projection (min_x,max_x) (padding,(s_x-.padding)) x
  and y = projection (min_y,max_y) (padding,(s_y-.padding)) y
  in (int_of_float x, int_of_float y)

let draw dom col ((x_min,x_max),(y_min,y_max)) =
  let l = graham_sort dom in
  let l = List.rev_map (to_coord (x_min,x_max) (y_min,y_max)) l in
  set_color col; fill_poly (Array.of_list l);
  set_color black; draw_poly (Array.of_list l)

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
