
open Bench_desc_j

let () =
  let data = `Rational in
  print_endline (string_of_bound data);
  let input = "\"Float\""  in
  let data' = bound_of_string input in
  print_endline (string_of_bound data')