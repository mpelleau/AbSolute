(* open Octagon *)
open Dbm
open Abstract_domain

(* I. Data and utilities *)

let x = { l=0; c=1 }
let y = { l=2; c=3 }
let xy = { l=2; c=0 }
let xy' = { l=2; c=1 }

module Octagon_tester (B:Bound_sig.BOUND) =
struct
  module BTester = Bound_tester.Make(B)
  include BTester

  (* this value is to distinguish the diagonal element in the tests (we do not want to check it is equal to 0 because the incremental closure does not set it to 0, but the closure does). *)
  let special = B.of_int_up 123456789

  let expect_dbm name dbm_list dbm_expected =
    List.iter2
      (fun num (expected, obtained) ->
         if B.neq expected special then
           expect_le (name ^ ".dbm[" ^ (string_of_int num) ^ "]") expected obtained)
      (Tools.range 0 ((List.length dbm_expected)-1))
      (List.combine dbm_expected dbm_list)
end

module OctagonZ = Octagon.OctagonZ(Octagon_split.First_fail_bisect)
module OctagonTesterZ = Octagon_tester(OctagonZ.B)
module Z = OctagonZ.B

let string_of_interval itv = Printf.sprintf "{lb=(%d,%d); ub=(%d,%d)}" itv.lb.l itv.lb.c itv.ub.l itv.ub.c
let string_of_constraint c f = Printf.sprintf "(%d,%d) <= %s" c.v.l c.v.c (f c.d)
let string_of_kleene = function
 | True -> "True"
 | False -> "False"
 | Unknown -> "Unknown"

let make_octagon dim values =
  let vars = List.rev (Fold_intervals.fold (fun a x -> x::a) [] dim) in
  let vars = List.flatten (List.map (fun x -> x.lb::[x.ub]) vars) in
  let constraints = List.map2 (fun v d -> {v=v; d=d}) vars values in
  constraints

let octagon_2D = make_octagon 2
  [Z.of_int_up (-2); Z.of_int_up 10; (* bound of X *)
   Z.of_int_up 3; Z.of_int_up 2;     (* bound of X - Y, -X + Y *)
   Z.of_int_up (-3); Z.inf;          (* bound of -X - Y, X + Y*)
   Z.of_int_up (-2); Z.of_int_up 10; (* bound of Y *)]

let inf_dbm =
  [Z.inf; Z.inf;
   Z.inf; Z.inf;
   Z.inf; Z.inf; Z.inf; Z.inf;
   Z.inf; Z.inf; Z.inf; Z.inf]

let closed_dbm =
  [OctagonTesterZ.special; Z.of_int_up (-2);
   Z.of_int_up 10; OctagonTesterZ.special;
   Z.of_int_up 3; Z.of_int_up (-3); OctagonTesterZ.special; Z.of_int_up (-2);
   Z.of_int_up 10; Z.of_int_up 2; Z.of_int_up 10; OctagonTesterZ.special]

(* II. Tests *)

let test_dbm_var () =
begin
  Alcotest.(check bool) "inv(x)" true ((compare (inv x) { l=1; c=0 }) = 0);
  Alcotest.(check bool) "as_interval(x).lb" true ((compare (as_interval x).lb x) = 0);
  Alcotest.(check bool) "as_interval(x).ub" true ((compare (as_interval x).ub (inv x)) = 0);
  Alcotest.(check bool) "inv(xy)" true ((compare (inv xy) { l=3; c=1 }) = 0);
  Alcotest.(check bool) "as_interval(xy).lb" true ((compare (as_interval xy).lb xy) = 0);
  Alcotest.(check bool) "as_interval(xy).ub" true ((compare (as_interval xy).ub (inv xy)) = 0);
  Alcotest.(check bool) "inv(xy')" true ((compare (inv xy') { l=3; c=0 }) = 0);
  Alcotest.(check bool) "as_interval(xy').lb" true ((compare (as_interval xy').lb xy') = 0);
  Alcotest.(check bool) "as_interval(xy').ub" true ((compare (as_interval xy').ub (inv xy')) = 0);
  Alcotest.(check bool) "is_lower_bound(x)" true (is_lower_bound x);
  Alcotest.(check bool) "is_upper_bound(y)" true (is_upper_bound (inv x));
  Alcotest.(check bool) "is_lower_bound(xy)" true (is_lower_bound xy);
  Alcotest.(check bool) "is_upper_bound(xy')" true (is_upper_bound (inv xy));
end

let test_folder name f dim expected =
  let intervals = List.rev (f (fun a x -> x::a) [] dim) in
  List.iter2 (fun obtained expected ->
    Printf.printf "obtained=%s -- expected=%s\n" (string_of_interval obtained) (string_of_interval expected);
    Alcotest.(check bool) name true ((compare obtained expected) = 0);
  )
  intervals
  expected

let test_fold_intervals () =
  test_folder "Fold_intervals.fold" Fold_intervals.fold 2
    [{lb=x; ub=inv x}; {lb=xy;ub=inv xy}; {lb=xy';ub=inv xy'}; {lb=y;ub=inv y}]

let test_fold_intervals_canonical () =
  test_folder "Fold_intervals_canonical.fold" Fold_intervals_canonical.fold 2
    [{lb=x; ub=inv x}; {lb=y;ub=inv y}]

let test_fold_intervals_rotated () =
  test_folder "Fold_intervals_rotated.fold" Fold_intervals_rotated.fold 2
    [{lb=xy;ub=inv xy}; {lb=xy';ub=inv xy'}]

let test_octagon_Z () =
  let octagon = OctagonZ.init 2 in
  let constraints = octagon_2D in
  let octagon = List.fold_left OctagonZ.weak_incremental_closure octagon constraints in
  begin
    OctagonTesterZ.expect_dbm "init(Z)" (OctagonZ.DBM.to_list (OctagonZ.unwrap octagon)) inf_dbm;
    let octagon = OctagonZ.closure octagon in
    OctagonTesterZ.expect_dbm "closure(Z)" (OctagonZ.DBM.to_list (OctagonZ.unwrap octagon)) closed_dbm;
  end

let test_octagon_incremental_Z () =
  let octagon = OctagonZ.init 2 in
  let constraints = octagon_2D in
  let octagon = List.fold_left OctagonZ.incremental_closure octagon constraints in
  (OctagonZ.print Format.std_formatter octagon;
  OctagonTesterZ.expect_dbm "closure(Z)" (OctagonZ.DBM.to_list (OctagonZ.unwrap octagon)) closed_dbm)

let test_octagon_entailment_inf_Z () =
  let octagon = OctagonZ.init 2 in
  let constraints = octagon_2D in
  List.iter (fun c ->
    let obtained = OctagonZ.entailment octagon c in
    let expected = if c.d = Z.inf then True else Unknown in
    Printf.printf "Entailment of %s\n" (string_of_constraint c Z.to_string);
    Alcotest.(check bool) "entailment_inf(Z)" true (obtained = expected)) constraints

let test_octagon_entailment_Z () =
  let octagon = OctagonZ.init 2 in
  let constraints = octagon_2D in
  let octagon = List.fold_left OctagonZ.weak_incremental_closure octagon constraints in
  let octagon = OctagonZ.closure octagon in
  let entailment_constraints = [
    {v=x; d=Z.of_int_up (-1)};
    {v=x; d=Z.of_int_up (-2)};
    {v=x; d=Z.of_int_up (-3)};
    {v=x; d=Z.of_int_up (-10)};
    {v=x; d=Z.of_int_up (-11)};
  ] in
  let entailment_result = [True; True; Unknown; Unknown; False] in
  List.iter2 (fun c expected ->
    let obtained = OctagonZ.entailment octagon c in
    Printf.printf "Entailment of %s (obtained %s/expect %s)\n" (string_of_constraint c Z.to_string) (string_of_kleene obtained) (string_of_kleene expected);
    Alcotest.(check bool) "entailment(Z)" true (obtained = expected)) entailment_constraints entailment_result

let tests = [
  "dbm_var", `Quick, test_dbm_var;
  "fold_intervals", `Quick, test_fold_intervals;
  "fold_intervals_canonical", `Quick, test_fold_intervals_canonical;
  "fold_intervals_rotated", `Quick, test_fold_intervals_rotated;
  "init-closure(Z)", `Quick, test_octagon_Z;
  "incremental-closure(Z)", `Quick, test_octagon_incremental_Z;
  "entailment-inf(Z)", `Quick, test_octagon_entailment_inf_Z;
  "entailment(Z)", `Quick, test_octagon_entailment_Z;
]
