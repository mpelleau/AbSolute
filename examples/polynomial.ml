open Libabsolute

(* example of polynomial solving *)
let () =
  let open Csp in
  let prob =
    let csp = add_real_var_f "x" (-1000.) 1000. empty in
    let csp = add_constr csp (Parser.constr {|3*x^2 +4*x -5 <= 0|}) in
    csp
  in
  match Solver.Default.witness 0.000001 1000000 prob with
  | Witness i ->
      Format.printf "the polynomial admits (at least) one solution: %a\n"
        Instance.print i
  | Unfeasible -> Format.printf "the polynomial does not admit solutions\n"
  | Maybe -> Format.printf "the polynomial may admit solutions\n"
