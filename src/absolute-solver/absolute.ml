open Config
open Libabsolute
open Constant
open Parser

(* entry point *)
let _ =
  Random.init 0x4162536f6c757465 ;
  try
    parse_args () ;
    Terminal.go !problem ;
    if !debug > 0 then Vpl_domain.enable_debug () ;
    parse !problem |> run (Domains.parse !domain !boolean)
  with Error msg | IllFormedAST msg | Syntax_error msg -> Terminal.error msg
