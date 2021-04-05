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
  (* for debug *)
  (* Printexc.record_backtrace true ; *)
  (* Memtrace.trace_if_requested ~context:"absolute" () ; *)
  Random.init 0x4162536f6c757465 ;
  try
    let prob = Config.parse_args () in
    Terminal.go prob ;
    file prob |> Config.run (Domains.parse !domain !boolean)
  with Error msg | Semantic_error msg | Syntax_error msg ->
    Terminal.error msg ; exit 1
