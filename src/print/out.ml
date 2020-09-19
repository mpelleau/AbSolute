open Picasso

module Make (D:Signature.AbstractCP) = struct

  let build_render =
    let b = (150,150,255) in
    let g = (150,255,150) in
    fun abciss ordinate res ->
    let open Result in
    let r = Rendering.create ~abciss ~ordinate ~title:"AbSolute" 800. 800. in
    let r' = List.fold_left (fun acc (e,_) -> Rendering.add acc (b,D.render e)) r res.sure in
    List.fold_left (fun acc (e,_) -> Rendering.add acc (g,D.render e)) r' res.unsure

  let check_dir()=
    if not (Sys.file_exists "out" && Sys.is_directory "out") then begin
        Unix.mkdir "out" 0o777;
        Format.printf "directory ";
        Tools.cyan_fprintf Format.std_formatter "out";
        Format.printf " was created\n";
      end

  let traceout sure unsure =
    List.iter (fun (e, c) ->
        if D.is_empty e then
          Format.printf "sure: (), (%a)\n%!" Csp_printer.print_all_csts c
        else
          Format.printf "sure: (), (%a %a)\n%!" D.print e Csp_printer.print_all_csts c) sure;
    List.iter (fun (e, c) -> Format.printf "unsure: (%a), (%a)\n%!" D.print e Csp_printer.print_all_csts c) unsure

  let vars2D prob =
    let vars = Csp_helper.get_vars prob |> Array.of_list in
    let size = Array.length vars in
    (vars.(0)),(vars.(1 mod size))

  let vars3D prob =
    let vars = Csp_helper.get_vars prob |> Array.of_list in
    let size = Array.length vars in
    (vars.(0)),(vars.(1 mod size)),(vars.(2 mod size))

  (* text output on std out *)
  let terminal_output fmt res =
    let open Result in
    (match res.sure with
     | [] -> Format.fprintf fmt "No inner solutions found\n"
     | l ->
        Format.fprintf fmt "Inner solutions:";
        if !Constant.trace then begin
            Format.printf "\n";
           List.iter (fun (e,_) -> Format.fprintf fmt "%a\n" D.print e) l
          end else
          Format.printf " %i\n" (res.nb_sure)
    );
    Format.fprintf fmt "Inner volume : %f\n" (res.vol_sure);
    (match res.unsure with
     | [] -> Format.fprintf fmt "No outter solutions found\n"
     | l ->
        Format.fprintf fmt "Outter solutions:";
        if !Constant.trace then begin
            Format.printf "\n";
            List.iter (fun (e,_) -> Format.fprintf fmt "%a\n" D.print e) l
          end else
          Format.printf " %i\n" (res.nb_unsure)
    );
    Format.fprintf fmt "Outer volume : %f\n" (res.vol_unsure);
    Format.fprintf fmt "Inner ratio : %a%%\n" Format.pp_print_float ((Result.inner_ratio res) *. 100.);
    Format.printf "solving time : %fs\n" (Sys.time ());
    if not (!Constant.trace) then
      Format.fprintf fmt "you can use the -trace (or -t) option to list the solutions\n"

  let trace_min sure unsure value =
    Format.printf "best value:%s\n%!" (Mpqf.to_string value);
    traceout sure unsure

  let out_min _prob res =
    Format.printf "\ntime : %fs\n" (Sys.time ());
    let open Result in
    let open Constant in
    let u = if !sure then [] else res.unsure in
    if !trace then trace_min res.sure u res.best_value

  let vars2D prob =
    let vars = Csp_helper.get_vars prob |> Array.of_list in
    let size = Array.length vars in
    (vars.(0)),(vars.(1 mod size))

  let out prob res =
    let open Constant in
    Tools.green_fprintf Format.std_formatter "Results:\n";
    Format.printf "%a\n%!" terminal_output res;
    if !visualization || !tex then
      let v1,v2 = vars2D prob in
      let render = build_render v1 v2 res in
      if !tex then to_latex render "name";
      if !visualization then in_gtk_canvas render
end
