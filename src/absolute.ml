open Config

(***************)
(* entry point *)
(***************)

let go() =
  let open Constant in
  try
    parse_args ();
    Terminal.go();
    let prob = File_parser.parse !problem in
    Format.printf "%a\n" Csp_printer.print prob;
    if !debug > 0 then Vpl_domain.enable_debug();
    lift (set_domain ()) prob
  with Error msg -> Terminal.error msg

let _ =
  (* Initializing random with the seed : AbSolute converted to hexa *)
  Random.init 0x4162536f6c757465;
  go()
