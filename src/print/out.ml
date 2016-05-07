open Adcp_sig

module Make(Abs:AbstractCP) = struct

  let color_full = Graphics.rgb 0 191 255
  let color_maybe = Graphics.green

  (* latex printing *)
  let print_latex values vars =
    List.iter (fun (abs,_) ->
      let points = Abs.points_to_draw abs vars in
      if points <> [] then (
	Format.printf "  \\filldraw[rose, fill opacity = 0.3] ";
	List.iter (fun (x,y) -> Format.printf "(%f, %f) -- " x y) (Vue.graham_sort points);
	Format.printf "cycle;@.";
      )
    ) values

  (* We first compute the bounds (rectangle) of what we'll draw.
     Then we draw *)
  let draw2d (res : (Abs.t * bool) list) vars =
    match res with
    | [] -> ()
    | (h,full)::tl -> 
      
      let draw abs info col vars =
	let points = Abs.points_to_draw abs vars in
	if points <> [] then Vue.draw points col info
        
      and join_info ((min_x,max_x),(min_y,max_y)) ((min_x',max_x'),(min_y',max_y')) = 
	let min_x = min min_x min_x' and max_x = max max_x max_x'
	and min_y = min min_y min_y' and max_y = max max_y max_y' in
	(min_x,max_x),(min_y,max_y)
      in 
      
      let info = List.fold_left 
	(fun a (b,_) ->
	  let pts = Abs.points_to_draw b vars in
	  if pts <> [] then join_info a (Vue.get_info pts) else a
	) 
	(Vue.get_info (Abs.points_to_draw h vars)) 
	tl 
      in
      List.iter (fun (a,b) -> 
	draw a info (if b then color_full else color_maybe) vars
      ) res;
      Vue.draw_end info

  let draw3d values vars =
    if !Constant.domain = "box" then
      let out = Filename.basename !Constant.problem in
      let out = ("out/"^(Filename.chop_extension out)^".obj") in
      Objgen.doit out values vars
    else Format.printf "obj generation only available with interval domain for now\n"

  let out values vars =
    let open Constant in
    if !visualization then begin 
      Vue.create_window 800 800;
      draw2d values vars;
    end;
    if !Constant.tex then print_latex values vars;
    if !Constant.visualization then draw2d values vars
    (* if !Constant.obj then draw3d values vars *)

end
