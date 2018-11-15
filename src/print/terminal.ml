open Constant

let error x = Tools.red_fprintf Format.std_formatter x

let warning x = Tools.yellow_fprintf Format.std_formatter x

(** checking that the options specified by the user are coherent *)
let check_options () =
  if !pruning then
    (match !domain with
    | "box" | "boxQ" | "oct" ->
       warning
         "\n-pruning option is not available with %s domain. You can use instead 'boxS', 'boxQS' or 'poly' domains\n" !domain;
    | _ -> ())

(** launching the terminal interface *)
let go () =
  Format.printf "%a\n" Tools.green_fprintf "----------------------------------";
  Format.printf "%a\n" Tools.green_fprintf "| Welcome to the AbSolute solver |";
  Format.printf "%a\n" Tools.green_fprintf "----------------------------------";
  Format.printf "\n";
  Format.printf "AbSolute is a constraint solver based on abstract domains. For more info, check out https://github.com/mpelleau/AbSolute\n\n";
  if Vpl_domain.available then
    Format.printf "  # VPL library has been successfully loaded.\n"
  else
    Format.printf "  # VPL library not found on your system. You can try to install it to use the VPL's polyhedra inside AbSolute.\n";
  check_options ();
  Format.printf "_____________________________________________________\n";
  Format.printf "\nSolving the problem: "; Tools.cyan_fprintf Format.std_formatter "%s\n\n" !problem

(** Terminal ouput for errors *)
let error msg =
  error "AbSolute error: ";
  Format.printf "%s\n" msg;
  Format.printf "Usage: absolute [options] [filename]\n";
  Format.printf "You can run absolute with the '--help' option to see the options list\n"
