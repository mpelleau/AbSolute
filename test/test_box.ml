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
module BoxFFB = Box_base(Box_split.First_fail_bisect)(B)

module type Bound_tester_sig =
sig
  module Box: Box_sig
  val expect_bound_eq: string -> string -> int -> int -> unit
  val expect_domain_eq: Box.t -> (string * int * int) list -> unit
end

module Bound_tester(Box: Box_sig) =
struct
  module Box = Box
  let expect_bound_eq name var expected obtained =
    let name = name ^ " bound of `" ^ var ^ "`" in
    Alcotest.(check int) name expected obtained

  let expect_domain_eq box expected =
    List.iter (fun (var, lb, ub) ->
      let (lb', ub') = Box.I.to_range (Box.get box var) in
      begin
        expect_bound_eq "lower" var lb (Box.I.B.to_int_down lb');
        expect_bound_eq "upper" var ub (Box.I.B.to_int_up ub') end)
    expected
end

(* III. Tests *)

let test_Z () =
  let (module BT : Bound_tester_sig) = (module Bound_tester(BoxFFB)) in
  let box = BT.Box.init ["x"; "y"] constraints_Z in
  begin
    BT.expect_domain_eq box [("x", B.minus_inf, B.inf); ("y", B.minus_inf, B.inf)];
    let box = BT.Box.closure box in
    let box_expected = [("x",-1,3); ("y",0,4)] in
    BT.expect_domain_eq box box_expected;
    Printf.printf "first closure succeeded.\n";
    let box = BT.Box.weak_incremental_closure box x_eq_one in
    BT.expect_domain_eq box box_expected;
    Printf.printf "weak incremental closure succeeded.\n";
    let box = BT.Box.closure box in
    BT.expect_domain_eq box [("x",1,1); ("y",0,2)];
    Printf.printf "second closure succeeded.\n";
  end

open Box_split
module Input_order_bisect = Make(Input_order)(Middle)(Bisect)
module Input_order_assign_lb = Make(Input_order)(Lower_bound)(Assign)

let test_split name (module Box: Box_sig) left_branch right_branch =
begin
  let (module BT: Bound_tester_sig) = (module Bound_tester(Box)) in
  let box = BT.Box.closure (BT.Box.init ["x"; "y"] (x_eq_one::constraints_Z)) in
  let boxes = BT.Box.split box in
  Alcotest.(check int) name 2 (List.length boxes);
  List.iter (fun box -> BT.expect_domain_eq box [("x",1,1); ("y",0,2)]) boxes;
  let boxes = List.map BT.Box.closure boxes in
  BT.expect_domain_eq (List.nth boxes 0) left_branch;
  Printf.printf "left branch succeeded.\n";
  BT.expect_domain_eq (List.nth boxes 1) right_branch;
  Printf.printf "right branch succeeded.\n";
end

let test_split_input_order_bisect () =
  test_split "input order bisect" (module Box_base(Input_order_bisect)(Bound_int))
    [("x",1,1); ("y",0,1)]
    [("x",1,1); ("y",2,2)]

let test_split_input_order_assign_lb () =
  test_split "input order assign LB" (module Box_base(Input_order_assign_lb)(Bound_int))
    [("x",1,1); ("y",0,0)]
    [("x",1,1); ("y",1,2)]

let tests = [
  "init-closure(Z)", `Quick, test_Z;
  "split-input-order-bisect(Z)", `Quick, test_split_input_order_bisect;
  "split-input-order-assign-lb(Z)", `Quick, test_split_input_order_assign_lb;
]
