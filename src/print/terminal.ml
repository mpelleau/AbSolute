open Constant

let go () =
  Format.printf "%a\n" Tools.green_fprintf "----------------------------------";
  Format.printf "%a\n" Tools.green_fprintf "| Welcome to the AbSolute solver |";
  Format.printf "%a\n" Tools.green_fprintf "----------------------------------";
  Format.printf "\n";
  if Vpl_domain.available then
    Format.printf "  # VPL library has been successfully loaded.\n"
  else
    Format.printf "  # VPL library not found on your system. You can try to install it to use the VPL's polyhedra inside AbSolute.\n";
  Format.printf "\nProblem: "; Tools.cyan_fprintf Format.std_formatter "%s\n\n" !problem


let error() =
  Format.printf "%a" Tools.red_fprintf "AbSolute error: ";
  Format.printf "No filename specified\n";
  Format.printf "Usage: absolute [options] [filename]\n";
  Format.printf "You can type 'absolute --help' to see the options list\n"
