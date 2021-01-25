open Libabsolute

let p1 =
  [0; 0] |> List.map Q.of_int |> List.combine ["x"; "y"] |> Instance.of_list

let p2 =
  [2; 2] |> List.map Q.of_int |> List.combine ["x"; "y"] |> Instance.of_list

let p3 =
  [3; 1] |> List.map Q.of_int |> List.combine ["x"; "y"] |> Instance.of_list

let p4 =
  [1; -1] |> List.map Q.of_int |> List.combine ["x"; "y"] |> Instance.of_list

let _ =
  let convex = Constraint.convex_hull [p1; p2; p3; p4] in
  Format.printf "system:\n%a\n" Constraint.print convex
