open Box_dom

(* I. Data *)

let x = Csp.Var "x"
let y = Csp.Var "y"

let constraints_Z =
  let open Csp in
  let constraints =
    [(x, GEQ); (* x >= -1 *)
     (x, LEQ); (* x <= 5 *)
     (y, GEQ); (* y >= 0 *)
     (y, LEQ); (* y <= 5 *)
     (Binary (ADD, x, y), LEQ)] (* x + y <= 3 *)
  in
  List.map2 (fun (l,op) r -> (l, op, Cst (Bound_rat.of_int r, Int)))
    constraints
    [-1; 5; 0; 5; 3]

let x_eq_one = Csp.(x, EQ, Cst (Bound_rat.one, Int))

(* II. Tests utilities *)

module B = Bound_int
module Box = BoxZ

let expect_bound_eq name var expected obtained =
  let name = name ^ " bound of `" ^ var ^ "`" in
  Alcotest.(check int) name expected obtained

let expect_domain_eq box expected =
  List.iter (fun (var, lb, ub) ->
    let (lb', ub') = Box.I.to_range (Box.get box var) in
    begin
      expect_bound_eq "lower" var lb lb';
      expect_bound_eq "upper" var ub ub' end)
  expected

(* III. Tests *)

let test_Z () =
  let box = Box.init ["x"; "y"] constraints_Z in
  begin
    expect_domain_eq box [("x", B.minus_inf, B.inf); ("y", B.minus_inf, B.inf)];
    let box = Box.closure box in
    let box_expected = [("x",-1,3); ("y",0,4)] in
    expect_domain_eq box box_expected;
    let box = Box.weak_incremental_closure box x_eq_one in
    expect_domain_eq box box_expected;
    let box = Box.closure box in
    expect_domain_eq box [("x",1,1); ("y",0,2)];
    let boxes = Box.split box in
    Alcotest.(check int) "input-order/assign branches number" 2 (List.length boxes);
    List.iter (fun box -> expect_domain_eq box [("x",1,1); ("y",0,2)]) boxes;
    let boxes = List.map Box.closure boxes in
    expect_domain_eq (List.nth boxes 0) [("x",1,1); ("y",0,0)];
    expect_domain_eq (List.nth boxes 1) [("x",1,1); ("y",1,2)];
  end

let tests = [
  "init-closure(Z)", `Quick, test_Z;
]
