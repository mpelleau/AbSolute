(*
   Tests for domains/boxed_octagon.ml
*)

open Boxed_octagon.BoxedOctagon

(* Some utilities to print names with argument of functions. *)
let tname2 (x,y) = "(" ^ string_of_int x ^ "," ^ string_of_int y ^ ")"
let fname name (arg1: int) = name ^ " " ^ string_of_int arg1
let fname2 name (arg1: int) (arg2: int) = fname name arg1 ^ " " ^ string_of_int arg2


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

let test_well_formed_base () =
  let name base =  "well_formed_base " ^ tname2 base in
  let test r base =
    Alcotest.(check bool) (name base) r (well_formed_base base) in
  List.iter2 test [true; true; false; false] [cbase; (0,1); (1,0); (1,1)]

let test_bound_pos name bound_pos r =
  let test_in_base r base =
    let name x y = name ^ string_of_int x ^ " " ^ tname2 y in
    List.iter2 (fun x r -> Alcotest.(check int) (name x base) r (bound_pos x base)) (range 0 2) r in
  List.iter2 test_in_base
    r
    [cbase; (0,1); (0,2); (1,2)]

let test_lb_pos () = test_bound_pos "lb_pos " lb_pos [[1;7;17]; [5;4;17]; [13;7;12]; [1;15;14]]
let test_ub_pos () = test_bound_pos "ub_pos " ub_pos [[2;10;22]; [8;9;22]; [18;10;19]; [2;20;21]]

let test_if_rotated_else () =
  let name var base = "if_rotated_else " ^ string_of_int var ^ " " ^ tname2 base in
  let test base r =
    let check v r = Alcotest.(check bool) (name v base) true (if_rotated_else v base r (not r)) in
    List.iter2 check (range 0 2) r in
  test cbase [false; false; false];
  test (0,1) [true; true; false]

let tests = [
  "matpos", `Quick, test_matpos;
  "matpos2", `Quick, test_matpos2;
  "well_formed_base", `Quick, test_well_formed_base;
  "lb_pos", `Quick, test_lb_pos;
  "ub_pos", `Quick, test_ub_pos;
  "if_rotated_else", `Quick, test_if_rotated_else;
]
