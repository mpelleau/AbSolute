(*
   Tests for domains/boxed_octagon.ml
*)

open Boxed_octagon.BoxedOctagon

(* Some utilities to print names with argument of functions. *)
let fname2 name (arg1: int) (arg2: int) = name ^ " " ^ string_of_int arg1 ^ " " ^ string_of_int arg2

let tname2 (x,y) = "(" ^ string_of_int x ^ "," ^ string_of_int y ^ ")"

let test_matpos_gen name matpos' =
begin
  List.iter (fun x -> Alcotest.(check int) (fname2 name 0 x) x (matpos' 0 x)) (range 0 10);
  List.iter2 (fun x r -> Alcotest.(check int) (fname2 name x 0) r (matpos' x 0)) (range 0 5) [0;2;4;8;12;18];
end

let test_matpos () = test_matpos_gen "matpos" matpos
let test_matpos2 () =
begin
  test_matpos_gen "matpos" matpos;
  List.iter2 (fun (x,y) r -> Alcotest.(check int) (fname2 "matpos2 " x y) r (matpos2 x y))
    [(0,2);(0,3);(1,2);(1,3)]
    [9;5;8;4]
end

let test_ub_pos () =
  let name x y = "ub_pos " ^ string_of_int x ^ " " ^ tname2 y in
  List.iter2 (fun x r -> Alcotest.(check int) (name x cbase) r (ub_pos x cbase)) (range 0 2) [2;10;22]

let tests = [
  "matpos", `Quick, test_matpos;
  "matpos2", `Quick, test_matpos2;
  "ub_pos", `Quick, test_ub_pos;
]
