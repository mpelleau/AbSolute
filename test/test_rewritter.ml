open Dbm
open Csp

module Rewritter = Octagonal_rewriting.Rewriter(Bound_int)

let x_i = (as_interval {l=0;c=1})
let xy_i = (as_interval {l=2;c=0})

let data = [("x", (as_interval {l=0;c=1}));
            ("y", (as_interval {l=2;c=3}));
            ("xy", (as_interval {l=2;c=0}))]

let ten = Cst (Bound_rat.of_int 10, Int)

let test_init () =
  let r = Rewritter.init data in
  begin
    Alcotest.(check bool) "init" true ((compare x_i (Rewritter.var_box_to_dbm r "x")) = 0);
    Alcotest.(check bool) "init" true ((compare xy_i (Rewritter.var_box_to_dbm r "xy")) = 0);
    Alcotest.(check bool) "init" true ((compare "x" (Rewritter.var_dbm_to_box r x_i)) = 0);
    Alcotest.(check bool) "init" true ((compare "xy" (Rewritter.var_dbm_to_box r xy_i)) = 0);
  end

let check_dbm_constraint name r c expected =
begin
  let obtained = Rewritter.rewrite r c in
  Alcotest.(check int) (name ^ "-length") (List.length obtained) (List.length expected);
  List.iter2 (fun obtained expected ->
    Printf.printf "interval obtained=%s / expected=%s\n" (Test_octagon.string_of_constraint obtained Rewritter.B.to_string) (Test_octagon.string_of_constraint expected Rewritter.B.to_string);
    Alcotest.(check bool) (name ^ "-interval") true ((compare obtained.v expected.v) = 0);
    Alcotest.(check bool) (name ^ "-constant") true (Rewritter.B.equal obtained.d expected.d);
  ) obtained expected
end

let test_rewrite () =
  let r = Rewritter.init data in
  begin
    check_dbm_constraint "rewrite x <= 10" r (Var "x", LEQ, ten) [{v={l=1;c=0}; d=(Rewritter.B.of_int_up 20)}];
    check_dbm_constraint "rewrite x >= 10" r (Var "x", GEQ, ten) [{v={l=0;c=1}; d=(Rewritter.B.of_int_up (-20))}];
    check_dbm_constraint "rewrite x < 10" r (Var "x", LT, ten) [{v={l=1;c=0}; d=(Rewritter.B.of_int_up 18)}];
    check_dbm_constraint "rewrite x > 10" r (Var "x", GT, ten) [{v={l=0;c=1}; d=(Rewritter.B.of_int_up (-22))}];
    check_dbm_constraint "rewrite x - y <= 10" r (Binary (SUB, Var "x", Var "y"), LEQ, ten) [{v={l=2;c=0}; d=(Rewritter.B.of_int_up 10)}];
    check_dbm_constraint "rewrite x - y > 10" r (Binary (SUB, Var "x", Var "y"), GT, ten) [{v={l=3;c=1}; d=Rewritter.B.of_int_up (-11)}];
    check_dbm_constraint "rewrite -x - y < 10" r (Binary (SUB, Unary (NEG, Var "x"), Var "y"), LT, ten) [{v={l=2;c=1}; d=Rewritter.B.of_int_up 9}];
    check_dbm_constraint "rewrite -x + y <= 10" r (Binary (SUB, Unary (NEG, Var "x"), Unary (NEG, Var "y")), LEQ, ten) [{v={l=3;c=1}; d=Rewritter.B.of_int_up 10}];
    check_dbm_constraint "rewrite x + y <= 10" r (Binary (SUB, Var "x", Unary (NEG, Var "y")), LEQ, ten) [{v={l=3;c=0}; d=Rewritter.B.of_int_up 10}];
  end

let tests = [
  "init", `Quick, test_init;
  "rewrite", `Quick, test_rewrite;
]
