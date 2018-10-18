(*
   Tests for domains/boxed_octagon.ml
*)

open Boxed_octagon.BoxedOctagon

let test_matsize () =
begin
  Alcotest.(check int) "matsize 0" (matsize 0) 0;
  Alcotest.(check int) "matsize 1" (matsize 1) 4;
  Alcotest.(check int) "matsize 2" (matsize 2) 12;
end

let tests = [ "matsize", `Quick, test_matsize; ]
