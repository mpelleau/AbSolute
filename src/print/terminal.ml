open Constant

let error x = Tools.red_fprintf Format.std_formatter x

let warning x = Tools.yellow_fprintf Format.std_formatter x

(** launching the terminal interface *)
let go () =
  Format.printf "%a\n" Tools.green_fprintf "----------------------------------";
  Format.printf "%a\n" Tools.green_fprintf "| Welcome to the AbSolute solver |";
  Format.printf "%a\n" Tools.green_fprintf "----------------------------------";
  Format.printf "\n";
  let l1 = "AbSolute is a constraint solver based on abstract domains." in
  let l2 = "For more info, check out \
            https://github.com/mpelleau/AbSolute" in
  let l3,l4 =
    if Vpl_domain.available then
      "VPL library has been successfully loaded.\n",""
    else
      "VPL library not found on your system.","You can try to install it to use the VPL's polyhedra inside AbSolute."
  in
  Format.printf "%s\n" l1;
  Format.printf "%s\n" l2;
  Format.printf "%s\n" l3;
  Format.printf "%s\n" l4;
  Format.printf "%s\n" String.(make (max (length l3) (length l4)) '-');
  Tools.green_fprintf Format.std_formatter "\nProblem building\n";
  Format.printf "problem: ";
  Tools.cyan_fprintf Format.std_formatter "%s\n" !problem

(** Terminal ouput for errors *)
let error msg =
  error "AbSolute error: ";
  Format.printf "%s\n" msg;
  Format.printf "Usage: absolute [options] [filename]\n";
  Format.printf "You can run absolute with the '--help' option to see the options list\n"
