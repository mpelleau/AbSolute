open Config

(***************)
(* entry point *)
(***************)

let go() =
  try
    parse_args ();
    Terminal.go();
    let prob = File_parser.parse !Constant.problem in
    Format.printf "%a\n" Csp.print prob;
    (if !Constant.debug > 0 then
      Vpl_domain.enable_debug();
      Printexc.record_backtrace true);
    lift (set_domain ()) prob
  with Constant.Error msg -> Terminal.error msg

let _ =
  (* Initializing random with the seed : AbSolute converted to hexa *)
  Random.init 0x4162536f6c757465;
  go()
