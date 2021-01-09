open Config

(* entry point *)
let _ =
  Random.init 0x4162536f6c757465 ;
  try
    parse_args () ;
    Terminal.go () ;
    let prob = File_parser.parse !Constant.problem in
    if !Constant.debug > 0 then Vpl_domain.enable_debug () ;
    run (Domains.parse !Constant.domain !Constant.boolean) prob
  with
  | Constant.Error msg
   |File_parser.IllFormedAST msg
   |File_parser.Syntax_error msg
  ->
    Terminal.error msg
