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
    in    let onlysure = aux v None sure in
          if !Constant.sure |> not then aux v onlysure unsure
          else onlysure

  let draw2d sure unsure (v1,v2) =
    View.create_window 800 800;
    let v1_b = bound_dim v1 sure unsure
    and v2_b = bound_dim v2 sure unsure in
    (match v1_b,v2_b with
     | None, None -> failwith "nothing to draw"
     | Some(a,b),None | None, Some(a,b) -> View.init ((Mpqf.to_float a), (Mpqf.to_float b)) (1.,1.)
     | Some(a,b),Some(c,d) -> View.init ((Mpqf.to_float a), (Mpqf.to_float b)) ((Mpqf.to_float c), (Mpqf.to_float d)));
    List.iter (fun a -> D.draw2d a (v1,v2) color_sure) sure;
    if !Constant.sure |> not then
      List.iter (fun a -> D.draw2d a (v1,v2) color_unsure) unsure;
    View.draw_end v1 v2

  let print_latex sure unsure (v1,v2) =
    Latex.create 8. 6.5;
    let v1_b = bound_dim v1 sure unsure
    and v2_b = bound_dim v2 sure unsure in
    (match v1_b,v2_b with
     | None, None -> failwith "nothing to draw"
     | Some(a,b),None | None, Some(a,b) -> Latex.init ((Mpqf.to_float a), (Mpqf.to_float b)) (1.,1.)
     | Some(a,b),Some(c,d) -> Latex.init ((Mpqf.to_float a), (Mpqf.to_float b)) ((Mpqf.to_float c), (Mpqf.to_float d)));
    let name = Filename.(basename !Constant.problem |> chop_extension) in
    let out = ("out/"^name^".tex") in
    let name = Latex.escape name in
    let fmt = Format.formatter_of_out_channel (open_out out) in
    Format.fprintf fmt "\\begin{figure}[t]\n\\centering\n\\begin{tikzpicture}[scale=0.6]\n";
    List.iter (fun a -> D.print_latex fmt a (v1,v2) color_sure) sure;
    if !Constant.sure |> not then
      List.iter (fun a -> D.print_latex fmt a (v1,v2) color_unsure) unsure;
    Format.fprintf fmt "\\end{tikzpicture}\n\\caption{%s}\n\\end{figure}\n" name

  (* generation of an .obj file for 3d viewing. Works with g3dviewer for example *)
  let draw3d values vars =
	  let out = Filename.basename !Constant.problem in
	  let out = ("out/"^(Filename.chop_extension out)^".obj") in
    let out = open_out out in
    let fmt = Format.formatter_of_out_channel out in
    D.draw3d fmt values vars

  let traceout sure unsure =
    List.iter (fun (e, c) ->
        if D.is_empty e then
          Format.printf "sure: (), (%a)\n%!" Csp.print_all_csts c
        else
          Format.printf "sure: (), (%a %a)\n%!" D.print e Csp.print_all_csts c) sure;
    List.iter (fun (e, c) -> Format.printf "unsure: (%a), (%a)\n%!" D.print e Csp.print_all_csts c) unsure

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

  (* text output on std out *)
  let terminal_output fmt res =
    let open Result in
    (match res.sure with
    | [] -> Format.fprintf fmt "No inner solutions found\n"
    | l ->
       Format.fprintf fmt "Inner solutions:";
       if !Constant.trace then begin
           Format.printf "\n";
           List.iter (fun (e,s) -> Format.fprintf fmt "%a\n" D.print e) l
         end else
         Format.printf " %i\n" (res.nb_sure)
    );
    (match res.unsure with
     | [] -> Format.fprintf fmt "No outter solutions found\n"
     | l ->
        Format.fprintf fmt "Outter solutions:";
        if !Constant.trace then begin
            Format.printf "\n";
            List.iter (fun (e,s) -> Format.fprintf fmt "%a\n" D.print e) l
          end else
          Format.printf " %i\n" (res.nb_unsure)
    );
    Format.fprintf fmt "Inner ratio : %2.f%%\n" (Result.inner_ratio res);
    Format.printf "solving time : %fs\n" (Sys.time ());
    if not (!Constant.trace) then
      Format.fprintf fmt "you can use the -trace (or -t) option to list the solutions\n"

  let out prob res =
    let open Result in
    let open Constant in
    Tools.green_fprintf Format.std_formatter "Results:\n";
    Format.printf "%a" terminal_output res;
    if !visualization || !tex || !obj then
      let s = List.rev_map D.to_abs res.sure in
      let u = if !sure then [] else List.rev_map D.to_abs res.unsure in
      if !visualization then draw2d s u (vars2D prob);
      if !tex then print_latex s u (vars2D prob);
      if !obj then draw3d s (vars3D prob)

  let trace_min sure unsure value =
    Format.printf "best value:%s\n%!" (Mpqf.to_string value);
    traceout sure unsure

  let out_min prob res =
    Format.printf "\ntime : %fs\n" (Sys.time ());
    let open Result in
    let open Constant in
    let (s, _) = List.split res.sure in
    let (u, c) = if !sure then ([], []) else List.split res.unsure in
    if !visualization then draw2d s u (vars2D prob);
    if !tex then print_latex s u (vars2D prob);
    if !obj then draw3d s (vars3D prob);
    let u = if !sure then [] else res.unsure in
    if !trace then trace_min res.sure u res.best_value

end
