open Bench_desc_j
open System
open Factory

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
  type t
  val solving_instrumented : Csp.prog -> (unit -> unit) -> t
  val solving : Csp.prog -> t
end

let measure_sample prob (module S: Solver_sig) _ =
  let start = Mtime_clock.counter () in
  ignore (S.solving prob);
  Mtime.Span.to_uint64_ns (Mtime_clock.count start)

exception TimeoutException

(* We warm up the CPU/caches/garbage collector with the current problem.
   In addition, we verify that this problem does not timeout. *)
let warm_up config prob (module S: Solver_sig) =
  let start = Mtime_clock.counter () in
  let check_timeout () =
    let elapsed_time = Mtime_clock.count start in
    if (Mtime.Span.to_s elapsed_time) > (float_of_int config.timeout) then
      raise TimeoutException
  in
  try
    ignore (S.solving_instrumented prob check_timeout);
    true
  with TimeoutException -> false

let absolute_problem_of_path config problem_path =
  match config.problem_kind with
  | `Absolute -> File_parser.parse problem_path
  | `PSPlib -> Rcpsp.Psplib.psp_to_absolute problem_path

let bench config problem_path domain precision =
  let measure = Measurement.init problem_path domain precision in
  let (module Abs) = make_abstract_domain domain in
  let (module S: Solver_sig) = (module Solver.Solve(Abs)) in
  let prob = absolute_problem_of_path config problem_path in
  Constant.set_prec precision;
  let measure =
    if warm_up config prob (module S) then
      let samples = List.map (measure_sample prob (module S)) (Tools.range 1 config.trials) in
      Measurement.process_samples measure samples
    else
      measure in
  Measurement.print_as_csv config measure

let iter_precision config problem_path domain =
  List.iter (bench config problem_path domain) config.precisions

let iter_domain config problem_path =
  List.iter (iter_precision config problem_path) config.domains

let extension_of_problem_kind config =
  match config.problem_kind with
  | `Absolute -> absolute_ext
  | `PSPlib -> psplib_ext

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
  iter_problem config

let () =
  let input_desc = get_bench_desc () in
  let config = extract_config_from_json input_desc in
  start_benchmarking config
