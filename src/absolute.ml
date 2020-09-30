open Config

(* entry point *)
let _ =
  if !Constant.debug > 0 then Printexc.record_backtrace true;
  Random.init 0x4162536f6c757465;
  let open Constant in
  try
    parse_args ();
    Terminal.go();
    let prob = File_parser.parse !problem in
    Format.printf "%a\n" Csp_printer.print prob;
    if !debug > 0 then Vpl_domain.enable_debug();
    lift (set_domain ()) prob
  with Error msg -> Terminal.error msg
