open Config

(***************)
(* entry point *)
(***************)

let go() =
  let open Constant in
  parse_args ();
  if !problem <> "" then begin
      Terminal.go();
      let prob = File_parser.parse !problem in
      Format.printf "%a\n" Csp.print prob;
      if !debug > 0 then Vpl_domain.enable_debug();
      lift (set_domain ()) prob
    end
  else Terminal.error()

let _ =
  (* Initializing random with the seed : AbSolute converted to hexa *)
  Random.init 0x4162536f6c757465;
  go()
