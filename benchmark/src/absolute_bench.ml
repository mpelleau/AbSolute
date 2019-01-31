open Bench_desc_j
open System
open Factory
open Strategy

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
  Mtime.Span.to_uint64_ns (State.statistics global).elapsed

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

let absolute_problem_of_path config problem_path =
  match config.problem_kind with
  | `Absolute -> File_parser.parse problem_path
  | `PSPlib -> Rcpsp.Psplib.psp_to_absolute problem_path
  | `Patterson -> Rcpsp.Psplib.patterson_to_absolute problem_path

let bench config problem_path domain precision =
  try
    Constant.set_prec precision;
    let (module Abs) = make_abstract_domain domain in
    let (module S: Solver_sig) = (module Solver.Solve(Abs)) in
    let prob = absolute_problem_of_path config problem_path in
    let stats, timed_out = warm_up config prob (module S) in
    let measure = Measurement.init stats problem_path domain precision in
    let measure = if timed_out then
      measure
    else
      let samples = List.map (measure_sample prob (module S)) (Tools.range 1 config.trials) in
      Measurement.process_samples measure samples in
    Measurement.print_as_csv config measure
  with e ->
    Measurement.print_exception problem_path (Printexc.to_string e)

let iter_precision config problem_path domain =
  List.iter (bench config problem_path domain) config.precisions

let iter_domain config problem_path =
  List.iter (iter_precision config problem_path) config.domains

let extension_of_problem_kind config =
  match config.problem_kind with
  | `Absolute -> absolute_ext
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
  let input_desc = get_bench_desc () in
  let config = extract_config_from_json input_desc in
  start_benchmarking config
