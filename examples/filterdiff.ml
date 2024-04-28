open Libabsolute
module B = Domains.Boolean.Make (Domains.BoxS)

let filter_n_print space c =
  match B.filter_diff space (B.internalize c) with
  | Sat -> Format.printf "sat"
  | Unsat -> Format.printf "unsat"
  | Filtered ((space', _, diff), _) ->
      Format.printf "filtered %a, variable modified: %a" B.print space'
        (Format.pp_print_list Format.pp_print_string)
        (List.of_seq (Tools.VarSet.to_seq diff))
  | Pruned _ -> Format.printf "pruned"

(* example of filtering a constraint and comput *)
let () =
  let open Csp in
  let prob =
    empty
    |> add_real_var_f "x1" (-1000.) 1000.
    |> add_real_var_f "x2" 0. 1000.
    |> add_real_var_f "x3" (-1000.) 1000.
  in
  let space = List.fold_left B.add_var B.empty prob.variables in
  let c1 = Constraint.(lt (Expr.var "x1") (Expr.var "x2")) in
  let c2 = Constraint.(lt (Expr.var "x2") (Expr.var "x3")) in
  filter_n_print space c1 ; Format.printf "\n" ; filter_n_print space c2
