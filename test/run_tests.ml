(*
   Run all the OCaml test suites defined in the project.
*)

let test_suites: unit Alcotest.test list = [
  "BoxedOctagon", Test_boxed_octagon.tests;
]

let () = Alcotest.run "AbSolute" test_suites
