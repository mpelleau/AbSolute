open Bench_desc_j
open System
open Rcpsp
open Rcpsp_model
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

module type RCPSP_sig =
sig
  include Abstract_domain.Abstract_domain
  val init_rcpsp: rcpsp_model -> t
end

(* This module benches a repository of files with the parametrized abstract domain. *)
module Bencher(Rcpsp_domain: RCPSP_sig) =
struct
  type t = {
    domain_name: string;
    config: benchmark;
    info: Measurement.global_info;
  }

  let init config domain_name = {
    config=config;
    domain_name=domain_name;
    info=Measurement.empty_info
  }

  let makespan rcpsp domain = Rcpsp_domain.project_one domain rcpsp.makespan

  (* I. Printing utilities. *)

  let print_variables domain vars =
    let vars = Rcpsp_domain.project domain vars in
    begin
      List.iter (fun (v, (l, u)) ->
        let (l, u) = (Rcpsp_domain.B.to_string l, Rcpsp_domain.B.to_string u) in
        Printf.printf "%s=[%s,%s] \n" v l u;
      ) vars;
    end
  let print_depth depth = List.iter (fun _ -> Printf.printf(".")) (Tools.range 0 (depth-1))
  let no_print _ _ _ _ = ()
  let print_node with_var status rcpsp depth domain =
  begin
    print_depth depth;
    Printf.printf "[%s][%f]" status (Rcpsp_domain.volume domain);
    if with_var then (
      print_variables domain rcpsp.octagonal_vars;
      print_variables domain rcpsp.box_vars);
    Printf.printf "\n";
    flush_all ()
  end

  let no_print_makespan _ _ = ()
  let print_makespan with_var rcpsp domain =
    let (lb,ub) = makespan rcpsp domain in
    begin
      Printf.printf "makespan: (%s,%s)\n" (Rcpsp_domain.B.to_string lb) (Rcpsp_domain.B.to_string ub);
      if with_var then
        print_variables domain rcpsp.octagonal_vars
    end

  (* II. Branch and bound support. *)

  let constraint_makespan rcpsp best domain =
    let open Csp in
    match best with
    | None -> domain
    | Some best ->
        let (_,ub) = makespan rcpsp best in
        let ub = Cst (Rcpsp_domain.B.to_rat ub, Int) in
        Rcpsp_domain.weak_incremental_closure domain (Var rcpsp.makespan, LT, ub)

  let solve bench stats print_node print_makespan rcpsp =
  begin
    let time_out = timeout_of_config bench.config in
    let rec aux depth best domain = begin
      (* Stop when we exceed the timeout. *)
      let open State in
      let elapsed = Mtime_clock.count stats.start in
      if (Mtime.Span.compare time_out elapsed) <= 0 then best else
      try
        let domain = constraint_makespan rcpsp best domain in
        let domain = Rcpsp_domain.closure domain in
        match Rcpsp_domain.state_decomposition domain with
        | False -> (print_node "false'" rcpsp depth domain; best)
        | True when (Rcpsp_domain.volume domain) = 1. ->
            (print_makespan rcpsp domain;
             print_node "true" rcpsp depth domain; Some domain)
        | x ->
            let status = match x with True -> "almost true" | Unknown -> "unknown" | _ -> failwith "unreachable" in
            print_node status rcpsp depth domain;
            let branches = Rcpsp_domain.split domain in
            List.fold_left (aux (depth+1)) best branches
      with Bot.Bot_found -> (print_node "false" rcpsp depth domain; best)
    end in
    let domain = Rcpsp_domain.init_rcpsp rcpsp in
    aux 0 None domain
  end

  (* III. Bench and processing of the results. *)

  let update_with_optimum rcpsp best measure =
    match best with
    | Some best ->
        let (lb, _) = makespan rcpsp best in
        let open Measurement in
        { measure with optimum = Some (Rcpsp_domain.B.to_rat lb) }
    | None -> measure

  let bench_problem bench problem_path =
    (* Dumb parameters for future improvements. *)
    let precision = 1. in
    let domain_kind = `BoxedOctagon `Integer in
    let config = bench.config in
    try
      let rcpsp = Rcpsp_model.create_rcpsp (make_rcpsp config problem_path) in
      let stats = State.init_global_stats () in
      let best = solve bench stats no_print no_print_makespan rcpsp in
      let stats = {stats with elapsed=Mtime_clock.count stats.start} in
      let measure = Measurement.init stats problem_path domain_kind precision in
      let measure = Measurement.update_time bench.config stats measure in
      let measure = update_with_optimum rcpsp best measure in
      Measurement.print_as_csv config measure;
      {bench with info=(Measurement.add_measure bench.info measure)}
    with e -> begin
      (* Printexc.print_backtrace stdout; *)
      Measurement.print_exception problem_path (Printexc.to_string e);
      {bench with info=(Measurement.add_erroneous_measure bench.info)}
    end

  let iter_problem bench =
    let problems = list_of_problems bench.config in
    List.fold_left bench_problem bench problems

  let start_benchmarking config domain_name =
  let bench = init config domain_name in
  begin
    Printf.printf "      << %s >>\n\n" domain_name;
    Measurement.print_csv_header bench.config;
    let bench = iter_problem bench in
    Measurement.print_bench_results bench.domain_name bench.info
  end
end

module RCPSP_Octagon(SPLIT: Octagon_split.Octagon_split_sig) =
struct
  include Box_octagon_disjoint.Make
    (Box_dom.Box_base(Box_split.First_fail_bisect))
    (Octagon.OctagonZ(SPLIT))

  let init_rcpsp rcpsp = init rcpsp.box_vars rcpsp.octagonal_vars rcpsp.constraints rcpsp.reified_bconstraints
end

module RCPSP_Box(SPLIT: Box_split.Box_split_sig) =
struct
  include Box_reified.BoxReifiedZ(SPLIT)

  let init_rcpsp rcpsp =
    let vars = (rcpsp.box_vars)@(rcpsp.octagonal_vars) in
    init vars rcpsp.constraints rcpsp.reified_bconstraints
end

module type Bencher_sig =
sig
  val start_benchmarking: benchmark -> string -> unit
end

let bench_box (module S: Box_split.Box_split_sig) config name =
  let (module M: RCPSP_sig) = (module RCPSP_Box(S)) in
  let (module B: Bencher_sig) = (module Bencher(M)) in
  B.start_benchmarking config name

let benchmark_suite_box config =
begin
  bench_box (module Box_split.First_fail_LB) config "Box(First_fail, LB)";
  (* bench_box (module Box_split.Anti_first_fail_LB) config "Box(Anti_first_fail, LB)";
  bench_box (module Box_split.Anti_first_fail_UB) config "Box(Anti_first_fail, UB)"; *)
end

let bench_octagon (module S: Octagon_split.Octagon_split_sig) config name =
  let (module M: RCPSP_sig) = (module RCPSP_Octagon(S)) in
  let (module B: Bencher_sig) = (module Bencher(M)) in
  B.start_benchmarking config name

let benchmark_suite_octagon config =
begin
  bench_octagon (module Octagon_split.MSLF) config "Octagon(MSLF)";
  bench_octagon (module Octagon_split.MSLF_all) config "Octagon(MSLF, all)";
  bench_octagon (module Octagon_split.MSLF_simple) config "Octagon(MSLF without tie breaking)";
  bench_octagon (module Octagon_split.Min_max_LB) config "Octagon(Min_max, LB)";
  bench_octagon (module Octagon_split.Max_min_LB) config "Octagon(Max_min, LB)";
  (* bench_octagon (module Octagon_split.Max_min_Bisect) config "Octagon(Max min, bisect)";
  bench_octagon (module Octagon_split.Anti_first_fail_LB_canonical) config "Octagon(Anti_first_fail, LB, Canonical)";
  bench_octagon (module Octagon_split.Anti_first_fail_UB_canonical) config "Octagon(Anti_first_fail, UB, Canonical)";
  bench_octagon (module Octagon_split.Anti_first_fail_LB) config "Octagon(Anti_first_fail, LB, All)";
  bench_octagon (module Octagon_split.Anti_first_fail_UB) config "Octagon(Anti_first_fail, UB, All)";
  bench_octagon (module Octagon_split.Max_min_UB) config "Octagon(Max_min, UB)";
  bench_octagon (module Octagon_split.Max_min_Bisect) config "Octagon(Max_min, Bisect)"; *)
end

let benchmark_solver config solver =
begin
  Printf.printf "\n  <<<< Benchmark suite for \"%s\" >>>>\n" config.problem_set;
  Printf.printf   "  <<<< Solver %s >>>>\n\n" (Measurement.name_of_solver solver);
  match solver with
  | `AbSolute ->
      (* benchmark_suite_box config; *)
      benchmark_suite_octagon config
  | `MiniZinc desc ->
      let desc = String.split_on_char '#' desc in
      Minizinc.benchmark_suite_minizinc config (List.nth desc 0) (List.nth desc 1)
  | `FlatMiniZinc desc ->
      let desc = String.split_on_char '#' desc in
      Minizinc.bench_flat_rcpsp config (List.nth desc 0) (List.nth desc 1)
end

let benchmark_suite config =
begin
  List.iter (benchmark_solver config) config.solvers
end

let () =
  (* Printexc.record_backtrace true; *)
  let input_desc = get_bench_desc () in
  let config = extract_config_from_json input_desc in
  benchmark_suite config
