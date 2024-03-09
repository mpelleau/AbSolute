open Libabsolute

let iter_directory f dir =
  if Sys.is_directory dir then
    Sys.readdir dir |> Array.map (( ^ ) (dir ^ "/")) |> Array.iter f
  else invalid_arg (dir ^ " is not a directory")

let _ =
  iter_directory
    (fun f ->
      Format.printf "Parsing %s\n" f ;
      let csp = Parser.file f in
      let nb_var = List.length (Csp.var_names csp) in
      let nb_cstr = List.length csp.Csp.constraints in
      if nb_var > 2 then
        let viz_name =
          "../../../out/" ^ string_of_int nb_var ^ "_" ^ string_of_int nb_cstr
          ^ "_"
          ^ Filename.basename (Filename.chop_suffix f "abs")
          ^ "viz"
        in
        Csp.to_graphviz csp viz_name )
    (* tests are run from _/build/default/tests/*)
    "../../../problems"
