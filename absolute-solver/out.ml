open Libabsolute
open Signature
open Picasso

module Make (D : Domain) = struct
  let print_list ?(t = false) msg print l =
    Format.printf "%i %s" (List.length l) msg ;
    if t then
      List.iteri (fun i -> Format.printf "\nsol n°%i:\n%a" (i + 1) print) l ;
    Format.printf "\n"

  let blue e = ((150, 150, 255), D.render e)

  let green e = ((150, 255, 150), D.render e)

  let build_render abciss ordinate res =
    let open Result in
    let r = Rendering.create ~abciss ~ordinate ~title:"AbSolute" 800. 800. in
    let r = Rendering.add_l r (List.map blue res.sure) in
    Rendering.add_l r (List.map green res.unsure)

  let build_render3d abciss ordinate height res =
    let open Result in
    let r = Rendering3d.create ~abciss ~ordinate ~height () in
    let r = Rendering3d.add_l r (List.map blue res.sure) in
    Rendering3d.add_l r (List.map green res.unsure)

  let witness w =
    let open Consistency in
    match w with
    | Unfeasible ->
        Format.printf "problem unsatisfiable.\n" ;
        exit 3
    | Maybe ->
        Format.printf "no solution found, but the problem maybe admits some.\n" ;
        exit 4
    | Witness i ->
        Format.printf "problem satisfiable.\nwitness value:\n%a\n"
          Instance.print i ;
        exit 5

  let satisfiability w =
    let open Kleene in
    match w with
    | False ->
        Format.printf "problem unsatisfiable.\n" ;
        exit 3
    | Unknown ->
        Format.printf "no solution found, but the problem maybe admits some.\n" ;
        exit 4
    | True ->
        Format.printf "problem satisfiable.\n" ;
        exit 5

  (* text output on std out *)
  let terminal_output ?(t = false) fmt res =
    let print_unsure fmt e = Format.fprintf fmt "%a\n" D.print e in
    let open Result in
    print_list ~t "inner solutions" D.print res.sure ;
    print_list ~t "outter solutions" print_unsure res.unsure ;
    Format.printf "total time : %fs\n" (Sys.time ()) ;
    if not !Constant.trace then
      Format.fprintf fmt
        "you can use the -trace (or -t) option to list the solutions\n" ;
    match (res.sure, res.unsure) with
    | [], [] ->
        Format.printf "problem unsatisfiable.\n" ;
        exit 3
    | [], _ :: _ ->
        Format.printf "no solution found, but the problem maybe admits some.\n" ;
        exit 4
    | _ :: _, _ ->
        Format.printf "problem satisfiable.\n" ;
        exit 5

  let vars2D prob =
    let vars = Csp.var_names prob |> Array.of_list in
    let size = Array.length vars in
    (vars.(0), vars.(1 mod size))

  let vars3D prob =
    let vars = Csp.var_names prob |> Array.of_list in
    let size = Array.length vars in
    (vars.(0), vars.(1 mod size), vars.(2 mod size))

  let output_name () =
    let open Constant in
    Format.asprintf "%s_%s_%i"
      Filename.(chop_extension !name |> basename)
      !domain !max_depth

  let results ?(t = false) prob res =
    let open Constant in
    ( if !obj then
        let v1, v2, v3 = vars3D prob in
        let render = build_render3d v1 v2 v3 res in
        to_obj render ~filename:"out/absolute.obj" ) ;
    if !visualization || !tex || !svg then (
      let v1, v2 = vars2D prob in
      let render = build_render v1 v2 res in
      let name = output_name () in
      if !tex then to_latex render ~filename:("out/" ^ name ^ ".tex") ;
      if !svg then to_svg render ~filename:("out/" ^ name ^ ".svg") ;
      if !visualization then show render ) ;
    Format.printf "%a\n%!" (terminal_output ~t) res
end
