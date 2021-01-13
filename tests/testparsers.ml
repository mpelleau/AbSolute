let iter_directory f dir =
  if Sys.is_directory dir then
    Sys.readdir dir |> Array.map ((^) (dir^"/")) |> Array.iter f
  else invalid_arg (dir^" is not a directory")

let _ =
  iter_directory
    (fun f -> Format.printf "Parsing %s\n" f; Libabsolute.Parser.parse f |> ignore)
    (* tests are run from _/build/default/tests/*)
    "../../../problems"
