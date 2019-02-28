open Bench_desc_j
open System
open Factory
open Strategy
open Rcpsp

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

let bench config problem_path _domain _precision =
  try
    let _rcpsp = make_rcpsp config problem_path in ()
    (* TODO: measure the solving strategy... *)
    (* Measurement.print_as_csv config measure *)
  with e ->
    Measurement.print_exception problem_path (Printexc.to_string e)

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
  let input_desc = get_bench_desc () in
  let config = extract_config_from_json input_desc in
  start_benchmarking config
