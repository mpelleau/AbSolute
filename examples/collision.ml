open Libabsolute

(* example of polynomial solving  *)

let () =
  let open Csp in
  let prob = Csp.empty
             |> add_real_var_f "x" (-1000.) 1000.
             |> add_constr (Parser.constr {| 3*x^2 +4x -5 = 0|})
  in
  Parser.semantic_check prob
