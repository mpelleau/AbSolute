open Libabsolute

let iter_directory f dir =
  if Sys.is_directory dir then
    Sys.readdir dir |> Array.map (( ^ ) (dir ^ "/")) |> Array.iter f
  else invalid_arg (dir ^ " is not a directory")

let _ =
  iter_directory
    (fun f ->
      let f_base = Filename.basename f in
      Format.printf "Parsing %s\n" f_base ;
      let csp = Parser.file f in
      let nb_var = List.length (Csp.var_names csp) in
      let nb_cstr = List.length csp.Csp.constraints in
      let out_dir = "../../../out/" in
      if nb_var > 2 then (
        if not (Sys.file_exists out_dir) then
          Sys.command (Format.asprintf "mkdir %s" out_dir) |> ignore ;
        let viz_name =
          Format.asprintf "%s%i_%i_%sviz" out_dir nb_var nb_cstr
            (Filename.chop_suffix f_base "abs")
        in
        Csp.to_graphviz csp viz_name ) )
    (* tests are run from _/build/default/tests/*)
    "../../../problems"
