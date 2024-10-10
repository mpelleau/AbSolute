open Libabsolute
module DA = Domains.Boolean.Make (Domains.Alias)
module DB = Domains.Boolean.Make (Domains.BoxS)

module AB = Domains.Product.Make (struct
  module A = DB
  module B = DA

  let product = None

  let spawn = None
end)

module S = Solver.Make (AB) (Iterator)

let () =
  let open Csp in
  let prob =
    let csp = add_int_var_i "x1" (-1000) 1000 empty in
    let csp = add_int_var_i "x2" (-1000) 1000 csp in
    let csp = add_int_var_i "x3" (-1000) 1000 csp in
    let csp = add_int_var_i "x4" (-1000) 1000 csp in
    let csp = add_int_var_i "x5" (-1000) 1000 csp in
    let csp = add_int_var_i "x6" (-1000) 1000 csp in
    let csp = add_int_var_i "x7" (-1000) 1000 csp in
    let open Constraint.Operators in
    let open Expr.Operators in
    let csp = add_constr csp (Expr.Var "x1" = Expr.Var "x2") in
    let csp = add_constr csp (Expr.Var "x3" = Expr.Var "x4") in
    let csp = add_constr csp (Expr.Var "x5" = Expr.Var "x6") in
    let csp = add_constr csp (Expr.Var "x3" = Expr.Var "x6") in
    let csp = add_constr csp (Expr.Var "x6" <= Expr.Var "x7" * Expr.Var "x7") in
    csp
  in
  match S.witness 0.0001 1000000 prob with
  | Witness i ->
      Format.printf "the problem admits (at least) one solution:@,%a\n"
        Instance.print i
  | Unfeasible -> Format.printf "the problem does not admit solutions\n"
  | Maybe -> Format.printf "the problem may admit solutions\n"
