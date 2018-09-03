(*****************************************************************)
(* this modules checks that the solver implementation works fine *)
(*****************************************************************)

module CheckBox = Checker.Make(Abstract_box.BoxF)

let print_sep () =
  Format.printf "----------------------------------------------------------\n"

let print_frontier fmt f =
  match f with
  | None -> Format.fprintf fmt "no frontier"
  | Some f -> Format.fprintf fmt "frontier: %.2f%%" (f *. 100.)

let print_good fmt () =
  Tools.green_fprintf fmt "\xE2\x9C\x94"

let print_not_bad fmt () =
  Tools.yellow_fprintf fmt "\xE2\x9C\x94"

let print_bad fmt () =
  Tools.red_fprintf fmt "\xE2\x9D\x8C"

let print_results not_bads goods nb_files =
  (if not_bads = nb_files then Tools.green_fprintf else Tools.red_fprintf)
    Format.std_formatter
    "success : %i/%i with %i confirmed\n"
    not_bads
    nb_files
    goods

(* returns the couple (g,b) where g are the known solutions of the problem
   and b are the known nogoods of a problem *)
let split sols =
  List.fold_left (fun (g,ng) (i,b) -> if b then (i::g,ng) else g,(i::ng))
    ([],[]) sols

let checkfiles dir files =
  print_sep();
  Constant.set_max_iter 10;
  let goods          = ref 0 in
  let not_bads       = ref 0 in
  let problem        = ref 0 in
  let output_infeasible arr prob res =
    if CheckBox.check_infeasible res then begin
        arr.(1) <- "infeasible";
        arr.(3) <- Format.asprintf "%a" print_good ();
        incr goods;
        incr not_bads;
      end
    else begin
        arr.(1) <- "infeasible, but solution found";
        arr.(3) <- Format.asprintf "%a" print_bad ();
        incr problem;
      end;
    arr.(2) <- Format.asprintf "----"
  in
  let output_feasible fn prob arr igoods ibads res =
    let known_sol = CheckBox.check_known_solutions fn res igoods in
    let known_bad = CheckBox.check_known_bad fn res ibads in
    let frontier = CheckBox.check_unsure fn prob res in
    let inner = CheckBox.check_sure fn prob res in
    arr.(1) <-
      (match inner, known_sol, known_bad with
       | _,false,_ ->
          arr.(3) <- Format.asprintf "%a" print_bad ();
          incr problem;
        Format.asprintf "covering element missing (false negative)"
       | _,_,false ->
          arr.(3) <- Format.asprintf "%a" print_bad ();
          incr problem;
          Format.asprintf "covering element exceeding (false positive)"
       | 0,true,true  ->
          arr.(3) <- Format.asprintf "%a" print_not_bad ();
          incr not_bads;
          Format.asprintf "0 solution"
       | nb_sol,true,true ->
          arr.(3) <- Format.asprintf "%a" print_good ();
          incr goods;
          incr not_bads;
          Format.asprintf "%i solutions" nb_sol);
    arr.(2) <- Format.asprintf "%a" print_frontier frontier
  in
  let output_msg fn =
    let arr = Array.make 4 "" in
    arr.(0) <- Format.asprintf "%s" fn;
    try
      let prob = File_parser.parse (dir^fn) in
      let res = CheckBox.result prob in
      (match prob.Csp.solutions with
      | None -> output_infeasible arr prob res
      | Some l -> let igoods,ibads = split l in
                  output_feasible fn prob arr igoods ibads res);
      arr
    with e ->
      let msg = Printexc.to_string e in
      arr.(1) <- Format.asprintf "the solver crashed :( ";
      arr.(2) <- msg;
      arr.(3) <- Format.asprintf "%a" print_bad ();
      arr
  in
  let mat = Array.map output_msg files in
  Format.printf "%a" Tools.matrix_print_indent mat;
  print_sep();
  print_results (!not_bads) (!goods) (Array.length files)

let _ =
  Random.self_init();
  let dir = "tests/" in
  Format.printf "regression test of the solver\n";
  Format.printf "using the %s domain\n" !Constant.domain;
  let files = Sys.readdir dir in
  checkfiles dir files
