open Libabsolute

(* example of polynomial solving *)
let () =
  let open Csp in
  let csp =
    add_real_var_f "v0" (-1000.) 1000. empty
    |> add_real_var_f "v1" (-1000.) 1000.
    |> add_real_var_f "v2" (-1000.) 1000.
    |> add_real_var_f "v3" (-1000.) 1000.
    |> add_real_var_f "v4" (-1000.) 1000.
  in
  let constr1 = Parser.constr "v0 <= v1 + v2 + v3 + v4" in
  let csp1 = Csp.add_constr csp constr1 in
  Csp.to_graphviz csp1 "diagdom.dot" ;
  let constr2a = Parser.constr "tmp1 = v1 + v2" in
  let constr2b = Parser.constr "tmp2 = v3 + v4" in
  let constr2c = Parser.constr "v0 <= tmp1 + tmp2" in
  let csp2 = Csp.add_constr csp constr2a in
  let csp2 = Csp.add_constr csp2 constr2b in
  let csp2 = Csp.add_constr csp2 constr2c in
  Csp.to_graphviz csp2 "diagdom2.dot"
