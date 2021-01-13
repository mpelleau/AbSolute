open Libabsolute
open Constant
open Parser

let parse (fn : string) =
  Format.printf "parsing ... %!" ;
  let p = Parser.file ~check:false fn in
  Format.printf "done.\nsemantic check ... %!" ;
  Parser.semantic_check p ;
  Format.printf "done.\n%!" ;
  p

(* entry point *)
let _ =
  Random.init 0x4162536f6c757465 ;
  try
    Config.parse_args () ;
    Terminal.go !problem ;
    file !problem |> Config.run (Domains.parse !domain !boolean)
  with Error msg | Semantic_error msg | Syntax_error msg -> Terminal.error msg
