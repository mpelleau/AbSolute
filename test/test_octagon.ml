open Octagon

(* I. Data *)

(* These octagons are presented in (The octagon abstract domain for continuous constraints, Pelleau et al., 2014). *)
let x = Csp.Var "x"
let y = Csp.Var "y"

let make_octagon constants =
  let open Csp in
  let constraints =           (* NOTE: the constants on the right depends on "constants". *)
    [(Unary (NEG, x), LEQ);         (* -x <= -1 *)
     (x, LEQ);                      (* x <= 5 *)
     (Unary (NEG, y), LEQ);         (* -y <= -1 *)
     (y, LEQ);                      (* y <= 5 *)
     (Binary (SUB, Unary (NEG, x), y), LEQ); (* -x - y <= -3 *)
     (Binary (SUB, y, x), LEQ);     (* y - x <= 2 *)
     (Binary (SUB, x, y), LEQ)] in  (* x - y <= 2.5 *)
  List.map2 (fun (l,op) r -> (l, op, Csp.Cst (r, Real))) constraints constants

module Q = Bound_rat

let blue_octagon =
  make_octagon [
    Q.of_int (-1); Q.of_int 5;
    Q.of_int (-1); Q.of_int 5;
    Q.of_int (-3); Q.of_int 2;
    Q.of_frac 5 2]

let blue_dbm =
  [Q.inf; Q.of_int (-2);
   Q.of_int 10; Q.inf;
   Q.of_frac 5 2; Q.of_int (-3); Q.inf; Q.of_int (-2);
   Q.inf; Q.of_int 2; Q.of_int 10; Q.inf]

let closed_blue_dbm =
  [Q.zero; Q.of_int (-2);
   Q.of_int 10; Q.zero;
   Q.of_frac 5 2; Q.of_int (-3); Q.zero; Q.of_int (-2);
   Q.of_int 10; Q.of_int 2; Q.of_int 10; Q.zero]

let dbmQ_to_dbmF dbm = List.map Bound_float.of_rat_up dbm
let dbmQ_to_dbmZ dbm = List.map Bound_int.of_rat_up dbm

(* II. Test utilities *)

module Octagon_tester
  (B:Bound_sig.BOUND)
  (Octagon: sig
    type t
    type bound
    val dbm_as_list: t -> bound list end with type bound=B.t) =
struct
  module BTester = Bound_tester.Make(B)
  include BTester

  let expect_dbm name octagon dbm_expected =
    List.iter2
      (fun num (expected, obtained) -> expect_le (name ^ ".dbm[" ^ (string_of_int num) ^ "]") expected obtained)
      (Tools.range 0 ((List.length dbm_expected)-1))
      (List.combine dbm_expected (Octagon.dbm_as_list octagon))
end

(* III. Tests *)

(* III.a Initialization of the DBM. *)

module OctagonZ_tester = Octagon_tester(Bound_int)(OctagonZ)
module OctagonQ_tester = Octagon_tester(Bound_rat)(OctagonQ)
module OctagonF_tester = Octagon_tester(Bound_float)(OctagonF)

let test_init_F () =
  let (_, octagon) = OctagonF.init ["x";"y"] blue_octagon in
  OctagonF_tester.expect_dbm "init(F)" octagon (dbmQ_to_dbmF blue_dbm)

let test_closure_F () =
  let (_, octagon) = OctagonF.init ["x";"y"] blue_octagon in
  OctagonF.closure octagon;
  OctagonF_tester.expect_dbm "closure(F)" octagon (dbmQ_to_dbmF closed_blue_dbm)

let test_init_Q () =
  let (_, octagon) = OctagonQ.init ["x";"y"] blue_octagon in
  OctagonQ_tester.expect_dbm "init(Q)" octagon blue_dbm

let test_closure_Q () =
  let (_, octagon) = OctagonQ.init ["x";"y"] blue_octagon in
  OctagonQ.closure octagon;
  OctagonQ_tester.expect_dbm "closure(Q)" octagon closed_blue_dbm

let test_init_Z () =
  let (_, octagon) = OctagonZ.init ["x";"y"] blue_octagon in
  OctagonZ_tester.expect_dbm "init(Z)" octagon (dbmQ_to_dbmZ blue_dbm)

let test_closure_Z () =
  let (_, octagon) = OctagonZ.init ["x";"y"] blue_octagon in
  OctagonZ.closure octagon;
  OctagonZ_tester.expect_dbm "closure(Z)" octagon (dbmQ_to_dbmZ closed_blue_dbm)

let test_entailment_Z () =
  let (_, octagon) = OctagonZ.init ["x";"y"] blue_octagon in
  OctagonZ.closure octagon;


let tests = [
  "init(Q)", `Quick, test_init_Q;
  "closure(Q)", `Quick, test_closure_Q;
  "init(F)", `Quick, test_init_F;
  "closure(F)", `Quick, test_closure_F;
  "init(Z)", `Quick, test_init_Z;
  "closure(Z)", `Quick, test_closure_Z;
  "entailment(Z)", `Quick, test_entailment_Z;
]
