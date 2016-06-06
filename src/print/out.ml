open Adcp_sig

module Make(Abs:AbstractCP) = struct

  let color_full = Graphics.rgb 0 191 255
  let color_maybe = Graphics.green

  (* latex printing *)
  let print_latex sure unsure vars =
    let print opacity = fun abs ->
      let points = Abs.points_to_draw abs vars in
      if points <> [] then (
	Format.printf "  \\filldraw[rose, fill opacity = %f] " opacity;
	List.iter (fun (x,y) -> Format.printf "(%f, %f) -- " x y) (Vue.graham_sort points);
	Format.printf "cycle;@.";
    ) 
    in 
    List.iter (print 0.3) sure; 
    List.iter (print 0.1) unsure

  (* We first compute the bounds (rectangle) of what we'll draw.
     Then we draw *)
  let draw2d sure unsure vars =
    Vue.create_window 800 800;
    let draw abs info col vars =
      let points = Abs.points_to_draw abs vars in
      if points <> [] then Vue.draw points col info
        
    and join_info ((min_x,max_x),(min_y,max_y)) ((min_x',max_x'),(min_y',max_y')) = 
      let min_x = min min_x min_x' and max_x = max max_x max_x'
      and min_y = min min_y min_y' and max_y = max max_y max_y' in
      (min_x,max_x),(min_y,max_y)
    in 

    let get_info = function
    | [] -> None
    | h::tl -> 
      let info = List.fold_left 
	(fun a b ->
	  let pts = Abs.points_to_draw b vars in
	  if pts <> [] then join_info a (Vue.get_info pts) else a
	) 
	(Vue.get_info (Abs.points_to_draw h vars)) 
	tl 
      in Some info
    in
    let info_all = 
      match (get_info sure),(get_info unsure) with
      | None, Some x | Some x, None -> x
      | Some y, Some x -> join_info x y
      | None, None -> assert false
    in
    List.iter (fun a -> 
      draw a info_all color_full vars
    ) sure;
    List.iter (fun a -> 
      draw a info_all color_maybe vars
    ) unsure;
    Vue.draw_end info_all

  let draw3d values vars =
    if !Constant.domain = "box" then
      match !Constant.problem with
      | None -> assert false
      | Some s ->
	let out = Filename.basename s in
	let out = ("out/"^(Filename.chop_extension out)^".obj") in
	Objgen.doit out values vars
    else Format.printf "obj generation only available with interval domain for now\n"

  let trace sure unsure = 
    List.iter (Format.printf "sure:%a\n%!" Abs.print) sure;
    List.iter (Format.printf "unsure:%a\n%!" Abs.print) unsure

  let out sure unsure vars =
    if !Constant.visualization then draw2d sure unsure vars;
    if !Constant.tex then print_latex sure unsure vars;
    if !Constant.trace then trace sure unsure
    (* if !Constant.obj then draw3d values vars *)

end
