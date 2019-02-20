open Dbm
open Octagon
open Printer
open Tools

let test_matpos_gen name matpos' =
begin
  List.iter (fun x -> Alcotest.(check int) (fname2 name 0 x) x (matpos' (0,x))) (range 0 10);
  List.iter2 (fun x r -> Alcotest.(check int) (fname2 name x 0) r (matpos' (x,0))) (range 0 5) [0;2;4;8;12;18];
end

let test_matpos () = test_matpos_gen "matpos" matpos
let test_matpos2 () =
begin
  test_matpos_gen "matpos" matpos;
  List.iter2 (fun (x,y) r -> Alcotest.(check int) (fname2 "matpos2 " x y) r (matpos2 (x,y)))
    [(0,2);(0,3);(1,2);(1,3)]
    [9;5;8;4]
end

let test_well_formed_plane () =
  let name plane =  "well_formed_plane " ^ tname2 plane in
  let test r plane =
    Alcotest.(check bool) (name plane) r (well_formed_plane plane) in
  List.iter2 test [true; true; false; false] [cplane; (0,1); (1,0); (1,1)]

let test_bound_pos fun_name bound_pos r =
  let test_in_plane r plane =
    let name = fname_key fun_name in
    List.iter2 (fun v r -> Alcotest.(check int) (name (v, plane)) r (matpos (bound_pos (v, plane)))) (range 0 2) r in
  List.iter2 test_in_plane
    r
    [cplane; (0,1); (0,2); (1,2)]

let test_lb_pos () = test_bound_pos "lb_pos " lb_pos [[1;7;17]; [5;4;17]; [13;7;12]; [1;15;14]]
let test_ub_pos () = test_bound_pos "ub_pos " ub_pos [[2;10;22]; [8;9;22]; [18;10;19]; [2;20;21]]

let test_if_rotated_else () =
  let name = fname_key "if_rotated_else " in
  let test plane r =
    let check v r = Alcotest.(check bool) (name (v, plane)) true (if_rotated_else (v, plane) r (not r)) in
    List.iter2 check (range 0 2) r in
  test cplane [false; false; false];
  test (0,1) [true; true; false]

let tests = [
  "matpos", `Quick, test_matpos;
  "matpos2", `Quick, test_matpos2;
  "well_formed_plane", `Quick, test_well_formed_plane;
  "lb_pos", `Quick, test_lb_pos;
  "ub_pos", `Quick, test_ub_pos;
  "if_rotated_else", `Quick, test_if_rotated_else;
]
