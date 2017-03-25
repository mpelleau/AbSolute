open Drawer_sig

module Make (D:Drawer) = struct

  let color_sure = Graphics.rgb 100 200 255
  let color_unsure = Graphics.green

  let bound_dim v sure unsure =
    let aux v info_init abs_list = List.fold_left (fun a b ->
      let (l,h) = D.bound b v in
      match a with
      | None -> Some(l,h)
      | Some(l',h') -> Some((min l l'),(max h h'))
	  ) info_init abs_list
    in
    let onlysure = aux v None sure in
    if !Constant.sure |> not then aux v onlysure unsure
    else onlysure

  let draw2d sure unsure (v1,v2) =
    View.create_window 800 800;
    let v1_b = bound_dim v1 sure unsure
    and v2_b = bound_dim v2 sure unsure in
    (match v1_b,v2_b with
    | None, None -> failwith "nothing to draw"
    | Some(a,b),None | None, Some(a,b) -> View.init (a,b) (1.,1.)
    | Some(a,b),Some(c,d) -> View.init (a,b) (c,d));
    List.iter (fun a -> D.draw2d a (v1,v2) color_sure) sure;
    if !Constant.sure |> not then
      List.iter (fun a -> D.draw2d a (v1,v2) color_unsure) unsure;
    View.draw_end ()

  let print_latex sure unsure (v1,v2) =
    match !Constant.problem with
    | None -> assert false
    | Some s ->
       let name = Filename.(basename s |> chop_extension) in
       let out = ("out/"^name^".tex") in
       let name = Latex.escape name in
       let fmt = Format.formatter_of_out_channel (open_out out) in
       Format.fprintf fmt "\\begin{figure}[t]\n\\centering\n\\begin{tikzpicture}[scale=0.6]\n";
       List.iter (fun a -> D.print_latex fmt a (v1,v2) color_sure) sure;
       if !Constant.sure |> not then
         List.iter (fun a -> D.print_latex fmt a (v1,v2) color_unsure) unsure;
       Format.fprintf fmt "\\end{tikzpicture}\n\\caption{%s}\n\\end{figure}\n" name

  let draw3d values vars =
    match !Constant.problem with
    | None -> assert false
    | Some s ->
	     let out = Filename.basename s in
	     let out = ("out/"^(Filename.chop_extension out)^".obj") in
       let out = open_out out in
       let fmt = Format.formatter_of_out_channel out in
	     D.draw3d fmt values vars

  let traceout sure unsure =
    List.iter (Format.printf "sure:%a\n%!" D.print) sure;
    List.iter (Format.printf "unsure:%a\n%!" D.print) unsure

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
    let open Constant in
    let unsure = if !sure then [] else res.unsure in
    if !visualization then draw2d res.sure unsure (vars2D prob);
    if !tex then print_latex res.sure unsure (vars2D prob);
    if !obj then draw3d res.sure (vars3D prob);
    if !trace then traceout res.sure unsure

  let trace_min sure unsure value =
    Format.printf "best value:%f\n%!" value;
    List.iter (Format.printf "sure:%a\n%!" D.print) sure;
    List.iter (Format.printf "unsure:%a\n%!" D.print) unsure

  let out_min sure unsure value vars =
    if !Constant.visualization then draw2d sure unsure vars;
    (* if !Constant.tex then print_latex sure unsure vars; *)
    if !Constant.trace then
      if !Constant.sure then trace_min sure [] value
			else trace_min sure unsure value
(* if !Constant.obj then draw3d values vars *)

end
