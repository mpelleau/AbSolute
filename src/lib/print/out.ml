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
      let r' =
        List.fold_left
          (fun acc e -> Rendering.add acc (b, D.render e))
          r res.sure
      in
      List.fold_left
        (fun acc e -> Rendering.add acc (g, D.render e))
        r' res.unsure

  let witness w =
    let open Kleene in
    match w with
    | False, None -> Format.printf "problem unsatisfiable.\n"
    | Unknown, None ->
        Format.printf "no solution found, but the problem maybe admits some.\n"
    | True, Some i ->
        Format.printf "problem satisfiable.\nwitness value:\n%a\n"
          Csp_printer.instance i
    | _ -> assert false

  let satisfiability w =
    let open Kleene in
    match w with
    | False -> Format.printf "problem unsatisfiable.\n"
    | Unknown ->
        Format.printf "no solution found, but the problem maybe admits some.\n"
    | True -> Format.printf "problem satisfiable.\n"

  let vars2D prob =
    let vars = Csp_helper.get_vars prob |> Array.of_list in
    let size = Array.length vars in
    (vars.(0), vars.(1 mod size))

  let results ?(t = false) prob res =
    let open Constant in
    Format.printf "%a\n%!" (terminal_output ~t) res ;
    if !visualization || !tex || !obj then (
      let v1, v2 = vars2D prob in
      let render = build_render v1 v2 res in
      if !tex then to_latex render "name" ;
      if !obj then failwith "picasso 3d" ;
      if !visualization then in_gtk_canvas render )
end
