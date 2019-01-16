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
  val solving : Csp.prog -> t
end

let bench_problem data_path domain =
  let (module Abs) = make_abstract_domain domain in
  let (module S: Solver_sig) = (module Solver.Solve(Abs)) in
  let prob = File_parser.parse data_path in
  let now = Mtime_clock.counter () in
  ignore (S.solving prob);
  let duration = Mtime_clock.count now in
  let measure = Measurement.({ data_path=data_path; duration=duration }) in
  Measurement.print_as_csv measure

let start_benchmarking config =
  Measurement.print_csv_header ();
  List.iter (bench_problem config.data_path) config.domains

let () =
  let input_desc = get_bench_desc () in
  let config = extract_config_from_json input_desc in
  start_benchmarking config
