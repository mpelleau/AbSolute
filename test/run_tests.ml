(*
   Run all the OCaml test suites defined in the project.
*)

let test_suites: unit Alcotest.test list = [
  (* "BoxedOctagon", Test_boxed_octagon.tests; *)
  "Octagon", Test_octagon.tests;
  (* "Octagon_utilities", Test_octagon_utilities.tests; *)
  "Box", Test_box.tests;
  "Rewriter", Test_rewriter.tests;
  "Split strategies", Test_split_strategies.tests;
]

let () = Alcotest.run "AbSolute" test_suites
