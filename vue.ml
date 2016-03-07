open Graphics
open Format
open Apron
open Utils

module Col = struct 
  let cpt = ref 0 and size = ref 50

  let n_colors n =
    let step = 0xFFFFFF / (n+2) in
    let arr = Array.make n 0x000000 |> Array.mapi (fun i e -> (i+1) * step) in
    Array.sort (fun _ _ -> (Random.int 3) -1) arr;
    arr
	
  let cols= ref [||]

  let init n = cols := n_colors n; size := n

  let col () =
    let c = !cols.(!cpt mod !size) in
    (* Format.printf "cpt:%i and col=%i\n" !cpt c; *)
    cpt := (!cpt + 1);
    c 
end

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
  let a = float a and b = float b and c = float c
  and d = float d and n = float n in
  let r = (d-.c)/.(b-.a) in
  (n-.a) *. r +. c |> int_of_float

let to_coord (min_x,max_x) (min_y,max_y) (x,y) =
  let padding = 150 in
  let x = projection (min_x,max_x) (padding,(size_x()-padding)) x
  and y = projection (min_y,max_y) (padding,(size_y()-padding)) y
  in (x,y)

let draw dom col ((x_min,x_max),(y_min,y_max)) =
  let l = List.rev_map (to_coord (x_min,x_max) (y_min,y_max)) dom in
  set_color col; fill_poly (Array.of_list l);
  set_color black; draw_poly (Array.of_list l)

let clear dom ((x_min,x_max),(y_min,y_max)) =
  set_color (white);
  let l = List.rev_map (to_coord (x_min,x_max) (y_min,y_max)) dom in
  fill_poly (Array.of_list l)

let draw_dom_list l =
  let merge_info ((x1,x2),(y1,y2)) ((x1',x2'),(y1',y2')) =
    ((min x1 x1', max x2 x2'),(min y1 y1',max y2 y2'))
  in
  match l with
  | h::tl ->
    let i = get_info h in
    let (x,y) = List.fold_left (fun a b -> get_info b |> merge_info a) i tl in
    let l = List.rev_map (List.rev_map (to_coord x y)) l in
    List.iter (fun e ->
      set_color (Col.col());
      let arr = (Array.of_list e) in
      fill_poly arr;
      set_color black;
      draw_poly arr
    ) l
  | _ -> assert false



let loop state = 
  loop_at_exit [] (fun _ -> ())

let create_window width height =
  Col.init 50;
  Format.sprintf " %ix%i" width height |> open_graph;
  set_window_title "AbSolute";
  loop ()
