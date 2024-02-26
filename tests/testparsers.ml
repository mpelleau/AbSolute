open Libabsolute

let iter_directory f dir =
  if Sys.is_directory dir then
    Sys.readdir dir |> Array.map (( ^ ) (dir ^ "/")) |> Array.iter f
  else invalid_arg (dir ^ " is not a directory")

let _ =
  iter_directory
    (fun f ->
      Format.printf "Parsing %s\n" f ;
      try Parser.file f |> ignore
      with _ -> Format.eprintf "file %s could not be parsed" f )
    (* tests are run from _/build/default/tests/*)
    "../../../problems"
