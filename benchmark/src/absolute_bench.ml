open Bench_desc_j
open System
open Factory
open Strategy
open Rcpsp
open Rcpsp_model

let extract_config_from_json json_data =
  try
    benchmark_of_string json_data
  with
  | Atdgen_runtime__Oj_run.Error(msg)
  | Yojson.Json_error(msg) ->
      eprintf_and_exit (Printf.sprintf
        "The benchmarks description file contains an error:\n\n\
         %s\n\n\
        [help] Be careful to the case: \"int\" is not the same as \"Int\".\n\
        [help] You can find a full example of the JSON format in benchmark/data/example.json." msg)

module type Solver_sig = sig
  module Abs : Adcp_sig.AbstractCP
  val execute_strategy : Csp.prog -> Strategy.t -> Abs.t State.global
end

let base_strategy =
  C (BoundSolutions,
  C (Statistics,
  C (Collect_solutions,
  C (Propagation,
  (* C (BoundPrecision, *)   (* does not work with j3_2.sm, no solution. *)
  A Branching))))

let dfs_search_with_timeout =
  C (DFS,
  C (BoundTime,
  base_strategy))

let dfs_search =
  C (DFS,
  base_strategy)

let measure_sample prob (module S: Solver_sig) _ =
  let global = S.execute_strategy prob dfs_search in
  (State.statistics global).elapsed

(* We warm up the CPU/caches/garbage collector with the current problem.
   In addition, we verify that this problem does not timeout. *)
let warm_up config prob (module S: Solver_sig) =
  let global = S.execute_strategy prob dfs_search_with_timeout in
  if config.print_solutions then begin
    let res = (State.solutions global) in
    Format.printf "%a\n" Csp.print prob;
    Printf.printf "Number of solutions: %d\n" (List.length res.sure);
    let print_sol (abs, constants) =
      List.iter
        (fun (kind, var) ->
            let (l, u) = S.Abs.var_bounds abs var in
            let kind = match kind with
            | Csp.Int -> "int"
            | Csp.Real -> "real" in
            let (l, u) = (Bound_rat.to_string l, Bound_rat.to_string u) in
            Printf.printf "%s:%s[%s,%s]\n" var kind l u) (S.Abs.vars abs);
      List.iter
        (fun (var, (l, u)) ->
            let (l, u) = (Bound_rat.to_string l, Bound_rat.to_string u) in
            Printf.printf "%s:_[%s,%s]\n" var l u) constants in
      List.iter print_sol res.sure
  end;
  let stats = (State.statistics global) in
  let elapsed_time = stats.elapsed in
  stats, (Mtime.Span.compare elapsed_time (State.timeout global) >= 0)

(* Precondition: Sanity checks on the file path are supposed to be already done, otherwise it can throw I/O related exceptions.
The files from PSPlib are also supposed to be well-formatted. *)
let psp_to_rcpsp (problem_path: string) : Rcpsp_model.rcpsp_model =
  let rcpsp = Sm_format.read_sm_file problem_path in
  Rcpsp_model.create_rcpsp rcpsp

let patterson_to_rcpsp (problem_path: string) : Rcpsp_model.rcpsp_model =
  let rcpsp = Patterson.read_patterson_file problem_path in
  Rcpsp_model.create_rcpsp rcpsp

let make_rcpsp config problem_path =
  match config.problem_kind with
  | `PSPlib -> psp_to_rcpsp problem_path
  | `Patterson -> patterson_to_rcpsp problem_path

let measure_time config prob (module S: Solver_sig) measure timed_out =
  let open Measurement in
  if timed_out then
    measure
  else
    (* If the number of trials is set to 0, we take the time measured during the warm-up phase. *)
    let samples =
      if config.trials = 0 then
        [measure.stats.elapsed]
      else
        List.map (measure_sample prob (module S)) (Tools.range 1 config.trials) in
    let samples = List.map Mtime.Span.to_uint64_ns samples in
    Measurement.process_samples measure samples

let bench_absolute config problem_path domain precision =
  Constant.set_prec precision;
  let (module Abs) = make_abstract_domain domain in
  let (module S: Solver_sig) = (module Solver.Solve(Abs)) in
  let prob = File_parser.parse problem_path in
  let stats, timed_out = warm_up config prob (module S) in
  let measure = Measurement.init stats problem_path domain precision in
  let measure = measure_time config prob (module S) measure timed_out in
  measure

module Rcpsp_domain = Box_octagon.Make
  (Bound_int)
  (Octagonalisation.NoRotation)
  (Octagon.OctagonZ)
  (Box_dom.BoxZ)

let print_variables rcpsp domain =
  let vars = Rcpsp_domain.project domain rcpsp.box_vars in
  begin
    List.iter (fun (v, (l, u)) ->
      let (l, u) = (Rcpsp_domain.B.to_string l, Rcpsp_domain.B.to_string u) in
      Printf.printf "%s=[%s,%s] \n" v l u;
    ) vars;
  end

let print_node rcpsp depth domain =
begin
  List.iter (fun _ -> Printf.printf(".")) (Tools.range 0 (depth-1));
  (match Rcpsp_domain.state_decomposition domain with
  | False -> Printf.printf "[false]"
  | True -> Printf.printf "[true]"
  | Unknown -> Printf.printf "[unknown]");
  Printf.printf "[%f]" (Rcpsp_domain.volume domain);
  print_variables rcpsp domain;
  Printf.printf "\n";
  flush_all ()
end

let makespan rcpsp domain = Rcpsp_domain.project_one domain rcpsp.makespan

let constraint_makespan rcpsp best domain =
  let (_,ub) = makespan rcpsp best in
  let (lb,_) = makespan rcpsp domain in
  let ub = Rcpsp_domain.B.sub_up ub Rcpsp_domain.B.one in
  Rcpsp_domain.meet_var domain rcpsp.makespan (lb, ub)

let solve rcpsp =
begin
  let rec aux depth best domain = (* if depth = 0 then best else aux (depth-1) best domain in *)
    (* print_node rcpsp depth domain; *)
    try
      let domain = constraint_makespan rcpsp best domain in
      let domain = Rcpsp_domain.closure domain in
      match Rcpsp_domain.state_decomposition domain with
      | False -> best
      | True when (Rcpsp_domain.volume domain) = 1. ->
          (* let (lb,ub) = makespan rcpsp domain in *)
          (* Printf.printf "makespan: (%s,%s)\n" (Rcpsp_domain.B.to_string lb) (Rcpsp_domain.B.to_string ub); *)
          domain
      | True | Unknown ->
          let branches = (Rcpsp_domain.split domain) in
          List.fold_left (aux (depth+1)) best branches
    with Bot.Bot_found -> best in
  let domain = (Rcpsp_domain.init rcpsp.box_vars rcpsp.octagonal_vars rcpsp.constraints rcpsp.reified_octagonal) in
  (* print_node rcpsp 0 domain; *)
  let domain = Rcpsp_domain.closure domain in
  (* print_node rcpsp 0 domain; *)
(*   let open Csp in
  List.iter (fun (e1,op,e2) -> Format.printf "%a\n" print_bexpr (Cmp (op,e1,e2))) rcpsp.constraints; *)
  (domain, aux 0 domain domain)
end

let bench config problem_path _domain _precision =
  try
    let rcpsp = make_rcpsp config problem_path in
    let (domain, best_bound) = solve rcpsp in
    Printf.printf "%s " problem_path;
    if domain <> best_bound then begin
      let (lb, _) = makespan rcpsp best_bound in
      Printf.printf "%s\n" (Rcpsp_domain.B.to_string lb);
(*       Printf.printf "Start dates: ";
      print_variables rcpsp best_bound *)
    end
    else
      Printf.printf "End solving without finding a solution!\n";
    flush_all ();
    (* Measurement.print_as_csv config measure *)
  with e -> begin
    Printexc.print_backtrace stdout;
    Measurement.print_exception problem_path (Printexc.to_string e)
  end

let iter_precision config problem_path domain =
  List.iter (bench config problem_path domain) config.precisions

let iter_domain config problem_path =
  List.iter (iter_precision config problem_path) config.domains

let extension_of_problem_kind config =
  match config.problem_kind with
  | `PSPlib -> psplib_ext
  | `Patterson -> patterson_ext

let check_problem_file_format config problem_path =
  let ext = extension_of_problem_kind config in
  if Sys.is_directory problem_path then begin
    print_warning ("subdirectory " ^ problem_path ^ " ignored.");
    false end
  else if (Filename.extension problem_path) <> ext then begin
    print_warning ("file \"" ^ problem_path ^
      "\" ignored (expected extension `" ^ ext ^ "`).");
    false end
  else
    true

let iter_problem config =
  if Sys.is_directory config.problem_set then
    let files = Sys.readdir config.problem_set in
    Array.sort compare files;
    Array.to_list files |>
    List.map (fun x -> config.problem_set ^ x) |>
    List.filter (check_problem_file_format config) |>
    List.iter (iter_domain config)
  else
    if check_problem_file_format config config.problem_set then
      iter_domain config config.problem_set

let start_benchmarking config =
  Measurement.print_csv_header config;
  Constant.set_timeout_sec config.timeout;
  iter_problem config

let () =
  Printexc.record_backtrace true;
  let input_desc = get_bench_desc () in
  let config = extract_config_from_json input_desc in
  start_benchmarking config
