open Libabsolute

(* example of polynomial solving *)
module Solver = Solver.Make (Domains.Boolean.Make (Domains.BoxS))

let () =
  let open Csp in
  let prob =
    empty
    |> add_real_var_f "x" (-1000.) 1000.
    |> add_constr (Parser.constr {|3*x^2 +4*x -5 <= 0|})
  in
  match Solver.witness 0.000001 1000000 prob with
  | Witness i ->
      Format.printf "the polynomial admits (at least) one solution: %a\n"
        Instance.print i
  | Unfeasible -> Format.printf "the polynomial does not admit solutions\n"
  | Maybe -> Format.printf "the polynomial may admit solutions\n"
