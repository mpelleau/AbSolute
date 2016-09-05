open Adcp_sig

module Make (Abs:AbstractCP) = struct

  let color_sure = Graphics.rgb 0 191 255
  let color_unsure = Graphics.green

  (* (\* latex printing *\) *)
  (* let print_latex sure unsure vars = *)
  (*   let print opacity = fun abs -> *)
  (*     let pts = D.vertices2D abs vars in *)
  (*     if pts <> [] then ( *)
	(*       Format.printf "  \\filldraw[rose, fill opacity = %f] " opacity; *)
	(*       List.iter (fun (x,y) -> *)
  (*         Format.printf "(%f, %f) -- " x y *)
  (*       ) (View.graham_sort pts); *)
	(*       Format.printf "cycle;@."; *)
  (*     ) *)
  (*   in *)
  (*   List.iter (print 0.3) sure; *)
  (*   List.iter (print 0.1) unsure *)

  (* We first compute the bounds (rectangle) of what we'll draw.
     Then we draw *)
  let draw2d sure unsure (v1,v2) =
    View.create_window 800 800;
    let draw abs info col =
      let pts = Abs.vertices2d abs (v1,v2) in
      if pts <> [] then View.draw pts col info

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
	           let pts = Abs.vertices2d b (v1,v2) in
	           if pts <> [] then join_info a (View.get_info pts) else a
	         )
	         (View.get_info (Abs.vertices2d h (v1,v2)))
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
      draw a info_all color_sure
    ) sure;
    if !Constant.sure |> not then List.iter (fun a ->
      draw a info_all color_unsure
    ) unsure(* ; *)
  (* View.draw_end info_all *)

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

  let draw_vars prob =
    let open Csp in
    Array.of_list
      (match prob.to_draw with
      | [] -> get_vars prob
      | l -> l)

  let vars2D prob =
    let vars = draw_vars prob in
    let size = Array.length vars in
    (vars.(0)),(vars.(1 mod size))

  let vars3D prob =
    let vars = draw_vars prob in
    let size = Array.length vars in
    ((vars.(0)),(vars.(1 mod size)),(vars.(2 mod size)))

  let out prob res =
    let open Result in
    let open Csp in
    let unsure = if !Constant.sure then [] else res.unsure in
    if !Constant.visualization then
      draw2d res.sure unsure (vars2D prob);
    (* if !Constant.tex then print_latex res.sure unsure prob.to_draw; *)
    if !Constant.trace then trace res.sure unsure
    (* if !Constant.obj then draw3d res.sure vars *)


  let trace_min sure unsure value =
    Format.printf "best value:%f\n%!" value;
    List.iter (Format.printf "sure:%a\n%!" Abs.print) sure;
    List.iter (Format.printf "unsure:%a\n%!" Abs.print) unsure

  let out_min sure unsure value vars =
    if !Constant.visualization then draw2d sure unsure vars;
    (* if !Constant.tex then print_latex sure unsure vars; *)
    if !Constant.trace then
      if !Constant.sure then trace_min sure [] value
			else trace_min sure unsure value
(* if !Constant.obj then draw3d values vars *)

end
