open Graphics

let padding = 5.
let x_min = ref 0.
and x_max = ref 0.
and y_min = ref 0.
and y_max = ref 0.

(* graham sort *)
let signF f = if f < 0. then -1. else 1.

let bas_gauche l =
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
  let p = bas_gauche l in
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
  let s_x = size_x() |> float_of_int and s_y = size_y() |> float_of_int in
  let a = projection (min_x,max_x) (padding, (s_x-.padding)) a
  and b = projection (min_y,max_y) (padding, (s_y-. (2. *. padding))) b
  in (int_of_float a, int_of_float b)

let normalize p =
  to_coord (!x_min,!x_max) (!y_min,!y_max) p

let fill_circle a b r =
  let a,b = normalize (a,b) in
  fill_circle a b r

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

let draw_line_x x col = draw_seg (x, 0.) (x, size_y() |> float_of_int) col

let draw_line_y y col = draw_seg (0., y) ((size_x() |> float_of_int
), y) col

let draw_string x y str col =
  set_color col;
  moveto (int_of_float x) (int_of_float y);
  draw_string str

let init (xl,xu) (yl,yu) =
  x_min := xl;
  x_max := xu;
  y_min := yl;
  y_max := yu

let draw_end () =
  let lg = rgb 200 200 200
  and sx = size_x() |> float_of_int
  and sy = size_y() |> float_of_int in
  draw_line_x padding lg;
  draw_string padding (padding/.2. -. 5.) (string_of_float !x_min) black;
  draw_line_x (sx-.padding) lg;
  draw_string (sx-.padding) (padding/.2. -. 5.) (string_of_float !x_max) black;
  draw_line_y padding lg;
  draw_string (padding/.2.-.5.) (padding+.5.) (string_of_float !y_min) black;
  draw_line_y (sy -. padding) lg;
  draw_string (padding/.2.-.5.) (sy-.padding+.5.) (string_of_float !y_max) black

let loop state =
  loop_at_exit [] (fun _ -> ())

let create_window width height =
  Format.sprintf " %ix%i" width height |> open_graph;
  set_window_title "AbSolute";
  loop ()
