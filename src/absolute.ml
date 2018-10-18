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

let _ = Random.init(527)

let _ = go()
