(* This module is an extension of the Arg module of the stdlib *)
(* It only adds functionnalities in it, nothing is removed *)

include Arg

(** adds the default float value to the description *)
let default_float descr fref =
  Format.asprintf "%s.@.\tDefault value is %a." descr Format.pp_print_float
    !fref

(** adds the default int value to the description *)
let default_int descr iref =
  Format.asprintf "%s.@.\tDefault value is %i." descr !iref

(** adds the default bool value to the description *)
let default_bool descr bref =
  Format.asprintf "%s. %s by default." descr
    (if !bref then "Enabled" else "Disabled")

(** adds the default bool value to the description *)
let default_string descr sref = Format.asprintf "%s. Default is %s" descr !sref

(** adds option value to the description *)
let options descr opts =
  Format.asprintf "%s.@.\tPossible values are %s." descr opts

type alias = string * string

(** Augments the options list with the aliases *)
let parse_args_aliases speclist (aliases : alias list) anonymous globaldescr =
  let find (alias, name) =
    let _, behav, descr = List.find (fun (n, _, _) -> n = name) speclist in
    (alias, behav, "Alias for " ^ name ^ ": " ^ descr)
  in
  let aliases = List.map find aliases in
  Arg.parse (speclist @ aliases) anonymous globaldescr
