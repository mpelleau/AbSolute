(*
   Tests for domains/boxed_octagon.ml
*)

open Boxed_octagon.BoxedOctagon
open Tools

(* 1. Some utilities to print names with argument of functions. *)

let tname2 (x,y) = "(" ^ string_of_int x ^ "," ^ string_of_int y ^ ")"
let tfname2 (x,y) = "(" ^ string_of_float x ^ "," ^ string_of_float y ^ ")"
let fname name (arg1: int) = name ^ " " ^ string_of_int arg1
let fname2 name (arg1: int) (arg2: int) = fname name arg1 ^ " " ^ string_of_int arg2
let string_of_key (v, plane) = string_of_int v ^ " " ^ tname2 plane
let fname_key name k = name ^ " " ^ string_of_key k

(* 2. Data generator. *)

let epsilon = 0.000000001
let make_octagon1 () = add_var empty (Real, "x")
let make_octagon2 () = add_var (make_octagon1 ()) (Real, "y")
let make_octagon3 () = add_var (make_octagon2 ()) (Real, "z")

(* Generate all planes (d1,d2) such that d1 < d2 <= dim. *)
let gen_all_planes_but_cplane : t -> plane list = fun o ->
  let dim = length o in
  if dim = 0 then []
  else
    let dim = dim - 1 in
    List.rev (List.fold_left (fun accu d1 ->
     List.fold_left (fun accu d2 -> (d1, d2)::accu) accu (range (d1+1) dim)
    ) [] (range 0 dim))

let gen_all_planes : t -> plane list = fun o ->
  cplane::(gen_all_planes_but_cplane o)

let gen_all_dim: t -> int list = fun o -> (range 0 ((length o)-1))

let gen_all_key: t -> key list = fun o ->
  let dims = gen_all_dim o in
  let all_dim accu plane =
    List.fold_left (fun accu v -> (v, plane)::accu) accu dims in
  let all_plane =
    List.fold_left all_dim [] (gen_all_planes o) in
  List.rev all_plane

let gen_octagon4 () = [empty; make_octagon1 (); make_octagon2 (); make_octagon3 ()]
let gen_bound () = [0.; 1.; 1.5; -1.; -1.5; F.minus_inf; F.inf; F.minus_one; F.sqrt_up 2.; F.sqrt_down 2.; F.neg (F.sqrt_up 2.); F.neg (F.sqrt_down 2.)]

let x01 = internal_name "x" 0 1
let y01 = internal_name "y" 0 1

(* Choose 5/3 because it is not representable in a float, and might generate rounding errors. *)
let frac5_3 = Mpqf.of_frac 5 3
let c = Csp.Cst (frac5_3, Real)
let x = Csp.Var "x"
let y = Csp.Var "y"
let x_leq_C = (x, Csp.LEQ, c)
let x_geq_C = (x, Csp.GEQ, c)
let x_leq_y = (x, Csp.LEQ, y)


let c_0 = Csp.Cst ((Mpqf.of_int 0), Real)
let c_1 = Csp.Cst ((Mpqf.of_int 0), Real)
let c_1000 = Csp.Cst ((Mpqf.of_int 1000), Real)
let c_m1 = Csp.Cst ((Mpqf.of_int (-1)), Real)
let c_5 = Csp.Cst ((Mpqf.of_int 5), Real)
let c_2 = Csp.Cst ((Mpqf.of_int 2), Real)
let c_2_5 = Csp.Cst ((Mpqf.of_frac 5 2), Real)
let c_m3 = Csp.Cst ((Mpqf.of_int (-3)), Real)

let make_mpelleau_octagon constants =
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

(* These octagons are presented in (The octagon abstract domain for continuous constraints, Pelleau et al., 2014). *)
let blue_octagon =
  make_mpelleau_octagon [
    Mpqf.of_int (-1); Mpqf.of_int 5;
    Mpqf.of_int (-1); Mpqf.of_int 5;
    Mpqf.of_int (-3); Mpqf.of_int 2;
    Mpqf.of_frac 5 2]

let pink_octagon =
  make_mpelleau_octagon [
    Mpqf.of_int (-3); Mpqf.of_int 7;
    Mpqf.of_int (-2); Mpqf.of_int 6;
    Mpqf.of_int (-6); Mpqf.of_int 1;
    Mpqf.of_frac 7 2]

let orange_octagon =
  make_mpelleau_octagon [
    Mpqf.of_int (-1); Mpqf.of_int 7;
    Mpqf.of_int (-1); Mpqf.of_int 6;
    Mpqf.of_int (-3); Mpqf.of_int 2;
    Mpqf.of_frac 7 2]

let green_octagon =
  make_mpelleau_octagon [
    Mpqf.of_int (-3); Mpqf.of_int 5;
    Mpqf.of_int (-2); Mpqf.of_int 5;
    Mpqf.of_int (-6); Mpqf.of_int 1;
    Mpqf.of_frac 5 2]

let inf_dbm =
  [F.inf; F.inf;
   F.inf; F.inf;
   F.inf; F.inf; F.inf; F.inf;
   F.inf; F.inf; F.inf; F.inf]

let blue_dbm_after_box_filtering =
  [F.inf; -2.;
   10.; F.inf;
   F.inf; F.inf; F.inf; -2.;
   F.inf; F.inf; 10.; F.inf]

let blue_dbm_after_box_filtering_and_closure =
  [0.; -2.;
   10.; 0.;
   4.; -2.; 0.; -2.;
   10.; 4.; 10.; 0.]

let blue_dbm =
  [F.inf; -2.;
   10.; F.inf;
   2.5; -3.; F.inf; -2.;
   F.inf; 2.; 10.; F.inf]

let blue_dbm_closure =
  [0.; -2.;
   10.; 0.;
   2.5; -3.; 0.; -2.;
   10.; 2.; 10.; 0.]

let blue_box_after_filter_closure =
  [("x", 1., 5.);
   ("y", 1., 5.);
   (x01, 1.414213562373095, 7.07106781187);
   (y01, -2.82842712475, 2.82842712475)]

let blue_box_after_filter =
  [("x", 1., 5.);
   ("y", 1., 5.);
   (x01, F.minus_inf, F.inf);
   (y01, F.minus_inf, F.inf)]

let blue_box_after_filter_octagonal =
  [("x", 1., 5.);
   ("y", 1., 5.);
   (x01, 2.1213203435596424, 7.07106781187);
   (y01, -1.76776695297, 1.41421356238)]

(* 3. Unit testing *)

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

let test_well_formed_plane () =
  let name plane =  "well_formed_plane " ^ tname2 plane in
  let test r plane =
    Alcotest.(check bool) (name plane) r (well_formed_plane plane) in
  List.iter2 test [true; true; false; false] [cplane; (0,1); (1,0); (1,1)]

let test_bound_pos fun_name bound_pos r =
  let test_in_plane r plane =
    let name = fname_key fun_name in
    List.iter2 (fun v r -> Alcotest.(check int) (name (v, plane)) r (bound_pos (v, plane))) (range 0 2) r in
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

let test_emptiness () =
  begin
  Alcotest.(check bool) "is_empty" true (is_empty (empty));
  Alcotest.(check bool) "is_empty" false (is_empty (make_octagon1 ()));
  Alcotest.(check bool) "is_empty" false (is_empty (make_octagon2 ()));
  end

let test_copy () =
  begin
  Alcotest.(check bool) "copy" true (is_empty (copy empty));
  Alcotest.(check bool) "copy" false (is_empty (copy (make_octagon1 ())));
  end

let test_add_var () =
  begin
  Alcotest.(check int) "add_var (0)" 0 (dbm_length empty);
  Alcotest.(check int) "add_var (1)" 4 (dbm_length (make_octagon1 ()));
  Alcotest.(check int) "add_var (2)" 12 (dbm_length (make_octagon2 ()));
  end

let test_bound_delta name expected obtained =
  if expected <> obtained then
    let delta = (F.abs (expected -. obtained)) in
    let name = name ^ ".(epsilon: " ^ (string_of_float epsilon) ^ ", delta: " ^ (string_of_float delta) ^ ")" in
    Alcotest.(check bool) name true (delta <= epsilon)

(* We test that the `obtained` bound is less or greater than (depending on `cmp`) the `expected` bound.
   We allow some rounding errors to occur, but they must be in the right direction (thus we do not lose potential solutions).
   In addition, we test that the delta between the bounds is not greater than `epsilon`. *)
let expect_bound fun_name cmp expected obtained =
  let name = fun_name ^ " (expected: " ^ (string_of_float expected) ^ ", obtained: " ^ (string_of_float obtained) ^ ")" in
  Alcotest.(check bool) name true (cmp expected obtained);
  test_bound_delta name expected obtained

let expect_ge fun_name = expect_bound (fun_name ^ ".expect_ge") (<=)
let expect_le fun_name = expect_bound (fun_name ^ ".expect_le") (>=)

let test_bound_init fun_name bound expected =
  let test_all_key o k =
    expect_ge fun_name expected (bound o k);
    expect_le fun_name expected (bound o k) in
  let test_all_octagon o =
    List.iter (test_all_key o) (gen_all_key o) in
  List.iter test_all_octagon (gen_octagon4 ())

let test_lb () = test_bound_init "lb " lb F.minus_inf
let test_ub () = test_bound_init "ub " ub F.inf

let test_set_and_get fun_name get set expect =
  let name k b = fun_name ^ ".set_and_get (bound: " ^ (string_of_float b) ^ ", key:" ^ (string_of_key k) ^ ")" in
  let set_and_get o b k =
    let o = copy o in
    set o k b;
    expect (name k b) b (get o k) in
  let test_all_key o b =
    List.iter (set_and_get o b) (gen_all_key o) in
  let test_all_bound o =
    List.iter (test_all_key o) (gen_bound ()) in
  List.iter test_all_bound (gen_octagon4 ())

let test_set_lb () = test_set_and_get "lb" lb set_lb expect_le
let test_set_ub () = test_set_and_get "ub" ub set_ub expect_ge

let sort_string = List.sort String.compare
let clean_vars vs = sort_string (List.map (fun (_,y) -> y) vs)

let test_vars_accesses name vars_access =
  List.iter2
    (fun expected o -> Alcotest.(check (list string)) name expected (vars_access o))
    [[]; ["x"]; ["x";"y"]; ["x";"y";"z"]]
    (gen_octagon4 ())

let test_vars () = test_vars_accesses "vars" (fun vs -> clean_vars (vars vs))
let test_all_vars () = test_vars_accesses "all_vars" (fun vs -> clean_vars (all_vars vs))

let add_all_planes o = List.fold_left add_plane o (gen_all_planes_but_cplane o)
let make_rotated_octagon_2 () = add_all_planes (make_octagon2 ())
let make_rotated_octagon_3 () = add_all_planes (make_octagon3 ())

let test_add_plane () =
  List.iter2
    (fun o expected -> Alcotest.(check (list string)) "all_vars_all_plane" expected (clean_vars (all_vars o)))
  [make_rotated_octagon_2 (); make_rotated_octagon_3 ()]
  [sort_string ["x"; "y"; x01; y01];
   sort_string ["x"; "y"; "z"; x01; internal_name "x" 0 2;
      y01; internal_name "y" 1 2;
      internal_name "z" 0 2; internal_name "z" 1 2]]

let cons_to_string cons =
  let constraints = List.map (fun (e1, op, e2) -> (Csp.Cmp (op, e1, e2))) cons in
  Format.fprintf Format.str_formatter "%a" Csp.print_constraints constraints;
  Format.flush_str_formatter ()

let expect_dbm name o dbm_expected =
  let dbm = list_of_dbm o in
  List.iter2
    (expect_le (name ^ ".dbm cell"))
    dbm_expected
    dbm

let test_filter filter' make constraints vars_expected dbm_expected : t =
  let o = make () in
  let o = List.fold_left filter' o constraints in
  print_out o;
  let name = cons_to_string constraints in
  (match dbm_expected with
  | None -> ()
  | Some dbm_expected -> expect_dbm name o dbm_expected);
  List.iter (fun (var, l, u) ->
    expect_le ("ub(" ^ var ^ "): " ^ name) u (ub' o var);
    expect_ge ("lb(" ^ var ^ "): " ^ name) l (lb' o var))
   vars_expected;
  o

(* This function filters the `constraints` in a octagon created with `make ()`.
   The lower and upper bounds on the variables are given in `vars_expected`.
   The constraints are filtered only one time from left to right. *)
let test_filter_cons_in_box' = test_filter filter_box

let test_filter_cons_in_box make constraints vars_expected dbm_expected =
  ignore (test_filter_cons_in_box' make constraints vars_expected dbm_expected)

let test_filter_in_box () =
  test_filter_cons_in_box make_octagon2 [x_leq_C] [("x", F.minus_inf, (Mpqf.to_float frac5_3))] None;
  test_filter_cons_in_box make_octagon2 [x_geq_C] [("x", (Mpqf.to_float frac5_3), F.inf)] None;
  test_filter_cons_in_box make_octagon2 [x_geq_C; x_leq_C]
    [("x", (Mpqf.to_float frac5_3), (Mpqf.to_float frac5_3));
     ("y", F.minus_inf, F.inf)] None;
  test_filter_cons_in_box make_octagon2 [x_geq_C; x_leq_C; x_leq_y]
    [("x", (Mpqf.to_float frac5_3), (Mpqf.to_float frac5_3));
     ("y", (Mpqf.to_float frac5_3), F.inf)] None;
  test_filter_cons_in_box make_octagon2 blue_octagon
    [("x", 1., 5.);
     ("y", 1., 5.)] None

let test_dbm_closure o before after =
  expect_dbm "dbm.before_closure" o before;
  let bagnara_o = strong_closure_bagnara o in
  expect_dbm "dbm.after_closure_bagnara" bagnara_o after;
  let mine_o = strong_closure_mine o in
  expect_dbm "dbm.after_closure_mine" mine_o after;
  bagnara_o

let test_dbm () =
  let o = make_rotated_octagon_2 () in
  set_lb o (0,cplane) 1.;
  set_ub o (0,cplane) 5.;
  set_lb o (1,cplane) 1.;
  set_ub o (1,cplane) 5.;
  set_lb o (0,(0,1)) 2.1213203435596424;
  set_ub o (0,(0,1)) F.inf;
  set_lb o (1,(0,1)) (-.1.7677669529663687);
  set_ub o (1,(0,1)) 1.414213562373095;
  ignore (test_dbm_closure o blue_dbm blue_dbm_closure)

let test_filter_on_rotated () =
  (* According to this test, the box abstract domain alone cannot propagate octagonal rotated constraints. *)
  let o1 = test_filter_cons_in_box' make_rotated_octagon_2 blue_octagon
    blue_box_after_filter
    (Some blue_dbm_after_box_filtering) in
  let o2 = test_filter_cons_in_box' (fun () -> o1) blue_octagon
    blue_box_after_filter
    (Some blue_dbm_after_box_filtering) in
  Alcotest.(check bool) "idempotent filtering" true (equal o1 o2 epsilon);
  let o3 = test_dbm_closure o2 blue_dbm_after_box_filtering blue_dbm_after_box_filtering_and_closure in
  let o4 = test_filter_cons_in_box' (fun () -> o3) blue_octagon
    blue_box_after_filter_closure
    (Some blue_dbm_after_box_filtering_and_closure) in
  Alcotest.(check bool) "idempotent filtering and closure" true (equal o3 o4 epsilon);
  let _ = test_filter filter (fun () -> o4) blue_octagon
    blue_box_after_filter_octagonal
    (Some blue_dbm_closure) in
  ()

(* This test is just to confirm that Box cannot prune domain just with a rotated constraint.
   Actually it is normal since the rotated constraint both depends on x and y, which are both unbounded at the time of the filtering. *)
let test_rotated_constraint_on_box () =
  let box = B.add_var B.empty (Real, "x0_1") in
  let box = B.add_var box (Real, "y0_1") in
  let (rv1, _) = symbolic_var_rotation ("x0_1", "y0_1") in
  let rcons = (rv1, Csp.LEQ, c_5) in
  let box = B.filter box rcons in
  let (l,u) = B.float_bounds box "x0_1" in
  expect_ge "filter.rotated(x <= 5).ub" F.inf u;
  expect_le "filter.rotated(x <= 5).lb" F.minus_inf l

let octagon_from constraints =
  List.fold_left filter (make_rotated_octagon_2 ()) constraints

let test_laws name a b c m =
  (* Let '+' be either join or meet *)
  (* a + b = c *)
  Alcotest.(check bool) name true (equal (m a b) c epsilon);
  (* commutative: a + b = b + a *)
  Alcotest.(check bool) (name ^ " commutative: ") true (equal (m b a) (m a b) epsilon);
  (* if a + b = c then a + c = c and c + b = c *)
  Alcotest.(check bool) (name ^ "a + c = c: ") true (equal (m a c) c epsilon);
  Alcotest.(check bool) (name ^ "c + b = c: ") true (equal (m c b) c epsilon);
  (* idempotent *)
  Alcotest.(check bool) (name ^ "idempotent1") true (equal (m (m a b) (m a b)) (m a b) epsilon);
  Alcotest.(check bool) (name ^ "idempotent2") true (equal (m a a) a epsilon);
  (* associativy *)
  Alcotest.(check bool) (name ^ "associativy") true (equal (m (m a b) c) (m a (m b c)) epsilon)

let test_absorption name a b c =
  Alcotest.(check bool) (name ^ "absorption1") true (equal (meet a (join a b)) a epsilon);
  Alcotest.(check bool) (name ^ "absorption2") true (equal (join c (meet c a)) c epsilon)

let test_join_meet () =
  let blue = octagon_from blue_octagon in
  let pink = octagon_from pink_octagon in
  let orange = octagon_from orange_octagon in
  let green = octagon_from green_octagon in
  let join_blue_pink = join blue pink in
  let meet_blue_pink = meet blue pink in
  Printf.printf "\nblue:\n"; print_out blue;
  Printf.printf "\npink:\n"; print_out pink;
  Printf.printf "\norange:\n"; print_out orange;
  Printf.printf "\ngreen:\n"; print_out green;
  Printf.printf "\njoin blue pink:\n"; print_out join_blue_pink;
  Printf.printf "\nmeet blue pink:\n"; print_out meet_blue_pink;
  test_laws "join(blue,pink)=orange" blue pink orange join;
  test_laws "meet(blue,pink)=green" blue pink green meet;
  test_laws "join(green,orange)=orange" green orange orange join;
  test_laws "meet(green,orange)=green" green orange green meet;
  test_absorption "blue pink orange" blue pink orange

let test_lb_y () =
  let open Csp in
  let o = filter (make_rotated_octagon_2 ()) (Unary (NEG, y), LEQ, Cst ((Mpqf.of_int (-1)), Real)) in
  expect_le "lb_y" (1.) (lb' o "y")

let blue_left =
  Csp.(Binary (ADD, x, y), LEQ, Cst ((Mpqf.of_frac 13 2), Real)) (* x + y <= c *)
  ::(make_mpelleau_octagon [
    Mpqf.of_int (-1); Mpqf.of_int 5;
    Mpqf.of_int (-1); Mpqf.of_int 5;
    Mpqf.of_int (-3); Mpqf.of_int 2;
    Mpqf.of_frac 5 2])

let blue_right =
  make_mpelleau_octagon [
    Mpqf.of_int (-1); Mpqf.of_int 5;
    Mpqf.of_int (-1); Mpqf.of_int 5;
    Mpqf.of_frac (-13) 2; Mpqf.of_int 2;
    Mpqf.of_frac 5 2]

let test_split_lf () =
  let blue = octagon_from blue_octagon in
  let right = octagon_from blue_right in
  let left = octagon_from blue_left in
  let splits = split_lf blue [] in
  match splits with
  | [left'; right'] ->
      test_laws "join(blue_left,blue_right): " left right blue join;
      test_laws "join after split: " left' right' blue join;
      let left' = strong_closure_bagnara left' in
      let right' = strong_closure_bagnara right' in
      test_laws "join after split and closure: " left' right' blue join;
      Printf.printf "\n\nLEFT expected: "; print_out left;
      Printf.printf "\n\nLEFT obtained: "; print_out left';
      Printf.printf "\n\nRIGHT expected: "; print_out right;
      Printf.printf "\n\nRIGHT obtained: "; print_out right';
      Alcotest.(check bool) "split_lf(left)" true (equal left left' epsilon);
      Alcotest.(check bool) "split_lf(right)" true (equal right right' epsilon)
  | _ -> Alcotest.(check bool) "expected a binary split for split_lf" true false

let test_to_bexpr () =
  let blue = octagon_from blue_octagon in
  let constraints = to_bexpr blue in
  Printf.printf "\n%s\n\n" (cons_to_string blue_octagon);
  print_out blue;
  Printf.printf "\n%s\n\n" (cons_to_string constraints);
  let blue' = test_filter filter make_rotated_octagon_2 constraints
    blue_box_after_filter_octagonal
    (Some blue_dbm_closure) in
  print_out blue';
  ()

let test_shape2d () =
  let point_equal (x,y) (x', y') =
    let name elem = elem ^ " with p1:" ^ (tfname2 (x,y)) ^ " p2:" ^ (tfname2  (x',y')) in
    test_bound_delta (name "x") x x';
    test_bound_delta (name "y") y y' in
  let blue = octagon_from blue_octagon in
  let points = shape2d blue ("x","y") in
  point_equal (List.nth points 0) (3.,5.);
  point_equal (List.nth points 1) (5.,5.);
  point_equal (List.nth points 2) (5.,5.);
  point_equal (List.nth points 3) (5.,2.5);
  point_equal (List.nth points 4) (3.5,1.);
  point_equal (List.nth points 5) (2.,1.);
  point_equal (List.nth points 6) (1.,2.);
  point_equal (List.nth points 7) (1.,3.)

let tests = [
  "matpos", `Quick, test_matpos;
  "matpos2", `Quick, test_matpos2;
  "well_formed_plane", `Quick, test_well_formed_plane;
  "lb_pos", `Quick, test_lb_pos;
  "ub_pos", `Quick, test_ub_pos;
  "if_rotated_else", `Quick, test_if_rotated_else;
  "emptiness", `Quick, test_emptiness;
  "copy", `Quick, test_copy;
  "add_var", `Quick, test_add_var;
  "lb", `Quick, test_lb;
  "ub", `Quick, test_ub;
  "set_lb", `Quick, test_set_lb;
  "set_ub", `Quick, test_set_ub;
  "vars", `Quick, test_vars;
  "all_vars", `Quick, test_all_vars;
  "add_plane", `Quick, test_add_plane;
  "filter", `Quick, test_filter_in_box;
  "dbm", `Quick, test_dbm;
  "rotated_constraint_on_box", `Quick, test_rotated_constraint_on_box;
  "filter_on_rotated", `Quick, test_filter_on_rotated;
  "lb_y", `Quick, test_lb_y;
  "join_meet", `Quick, test_join_meet;
  "split_lf", `Quick, test_split_lf;
  "to_bexpr", `Quick, test_to_bexpr;
  "shape2d", `Quick, test_shape2d;
]
