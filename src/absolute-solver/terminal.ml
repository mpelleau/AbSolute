(** terminal output with a color given in parameter restoring default color
    after use *)
let color_printf col x =
  Format.kasprintf (fun s -> Format.printf "%s%s%s" col s "\027[0m") x

(** red terminal output *)
let red_fprintf x = color_printf "\027[31m" x

(** blue terminal output *)
let cyan_fprintf x = color_printf "\027[36m" x

(** green terminal output *)
let green_fprintf x = color_printf "\027[32m" x

(** yellow terminal output *)
let yellow_fprintf x = color_printf "\027[33m" x

let error x =
  (* in case of non terminated print we skip a line *)
  Format.printf "\n" ; red_fprintf x

(** launching the terminal interface *)
let go problem =
  Format.printf "----------------------------------\n" ;
  Format.printf "| Welcome to the AbSolute solver |\n" ;
  Format.printf "----------------------------------\n" ;
  Format.printf "\n" ;
  let l1 = "AbSolute is a constraint solver based on abstract domains." in
  let l2 = "For more info, check out https://github.com/mpelleau/AbSolute" in
  let l3, l4 =
    if Vpl_domain.available then
      ("VPL library has been successfully loaded.\n", "")
    else
      ( "VPL library not found on your system."
      , "You can install it to use the VPL's polyhedra inside AbSolute." )
  in
  Format.printf "%s\n%s\n%s\n%s\n" l1 l2 l3 l4 ;
  Format.printf "%s\n" String.(make (max (length l3) (length l4)) '-') ;
  green_fprintf "\nProblem building\n" ;
  Format.printf "problem: " ;
  cyan_fprintf "%s\n" problem

(** Terminal ouput for errors *)
let error msg =
  error "AbSolute error: " ;
  Format.printf "%s\n" msg ;
  Format.printf "Usage: absolute [options] [filename]\n" ;
  Format.printf
    "You can run absolute with the '--help' option to see the options list\n"
