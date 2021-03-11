open Libabsolute
open Signature
open Picasso

module Make (D : Domain) = struct
  let print_list ?(t = false) msg print l =
    Format.printf "%i %s" (List.length l) msg ;
    if t then
      List.iteri (fun i -> Format.printf "\nsol n°%i:\n%a" (i + 1) print) l ;
    Format.printf "\n"

  (* text output on std out *)
  let terminal_output ?(t = false) fmt res =
    let print_unsure fmt e = Format.fprintf fmt "%a\n" D.print e in
    let open Result in
    print_list ~t "inner solutions" D.print res.sure ;
    print_list ~t "outter solutions" print_unsure res.unsure ;
    Format.printf "total time : %fs\n" (Sys.time ()) ;
    if not !Constant.trace then
      Format.fprintf fmt
        "you can use the -trace (or -t) option to list the solutions\n"

  let build_render =
    let b = (150, 150, 255) in
    let g = (150, 255, 150) in
    fun abciss ordinate res ->
      let open Result in
      let r = Rendering.create ~abciss ~ordinate ~title:"AbSolute" 800. 800. in
      let r =
        Rendering.add_l r (List.map (fun e -> (b, D.render e)) res.sure)
      in
      Rendering.add_l r (List.map (fun e -> (g, D.render e)) res.unsure)

  let build_render3d abciss ordinate height res =
    let open Result in
    let r = Rendering3d.create ~abciss ~ordinate ~height () in
    let r = Rendering3d.add_l r (List.map D.render res.sure) in
    Rendering3d.add_l r (List.map D.render res.unsure)

  let witness w =
    let open Kleene in
    match w with
    | False, None -> Format.printf "problem unsatisfiable.\n"
    | Unknown, None ->
        Format.printf "no solution found, but the problem maybe admits some.\n"
    | True, Some i ->
        Format.printf "problem satisfiable.\nwitness value:\n%a\n"
          Instance.print i
    | _ -> assert false

  let satisfiability w =
    let open Kleene in
    match w with
    | False -> Format.printf "problem unsatisfiable.\n"
    | Unknown ->
        Format.printf "no solution found, but the problem maybe admits some.\n"
    | True -> Format.printf "problem satisfiable.\n"

  let vars2D prob =
    let vars = Csp.get_var_names prob |> Array.of_list in
    let size = Array.length vars in
    (vars.(0), vars.(1 mod size))

  let vars3D prob =
    let vars = Csp.get_var_names prob |> Array.of_list in
    let size = Array.length vars in
    (vars.(0), vars.(1 mod size), vars.(2 mod size))

  let results ?(t = false) prob res =
    let open Constant in
    Format.printf "%a\n%!" (terminal_output ~t) res ;
    ( if !obj then
      let v1, v2, v3 = vars3D prob in
      let render = build_render3d v1 v2 v3 res in
      to_obj render "out/absolute.obj" ) ;
    if !visualization || !tex then (
      let v1, v2 = vars2D prob in
      let render = build_render v1 v2 res in
      if !tex then to_latex render "name" ;
      if !visualization then show render )
end
