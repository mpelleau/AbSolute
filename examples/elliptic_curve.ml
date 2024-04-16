open Libabsolute

(* (y²) = (x³) + ax +b *)

let constr a b =
  let vy = Expr.var "y" in
  let vx = Expr.var "x" in
  let open Constraint.Operators in
  let open Expr.Operators in
  vy * vy = (vx * vx * vx) + ((a * vx) + b)

let build_csp () =
  let csp =
    Csp.empty |> Csp.add_int_var_i "y" 0 10 |> Csp.add_int_var_i "x" 0 10
  in
  let constr = constr (Expr.of_int 1) (Expr.of_int 1) in
  Csp.add_constr csp constr

let () =
  let csp = build_csp () in
  Format.printf "%a\n" Csp.print csp
