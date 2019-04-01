open Printf
open System
open Factory
open Rcpsp
open Rcpsp_data
open Rcpsp_model
open Csp
open Bench_desc_j

let dzn_file = "tmp.dzn"
let mzn_file = "tmp.mzn"
let output_file = "out.txt"

let minizinc_options config solver =
  "--output-time " ^
  (Printf.sprintf "--time-limit %d " (config.timeout * 1000)) ^
  "--solution-separator \"\" " ^
  "--unsat-msg \"unsat\" " ^
  "--unbounded-msg \"unbounded\" " ^
  "--unknown-msg \"unknown\" " ^
  "--error-msg \"error\" " ^
  "--search-complete-msg \"complete\" " ^
  (* "--no-output-comments " ^ *)
  "--output-to-file " ^ output_file ^ " " ^
  "--solver " ^ solver

let create_file data name =
  let oc = open_out name in
  fprintf oc "%s\n" data;
  close_out oc

let create_dzn_file data = create_file data dzn_file
let create_mzn_file data = create_file data mzn_file

let update_with_optimum measure numbers =
  if numbers <> [] then
    let latest_bound = List.nth numbers ((List.length numbers) - 1) in
    Measurement.{ measure with optimum = Some (Bound_rat.of_int latest_bound) }
  else measure

let update_time config measure line_with_time status =
  let elapsed_time = Scanf.sscanf line_with_time "%% time elapsed: %d.%d s "
    (fun a b -> time_of_ms (a*1000 + b*10)) in
  let stats = Measurement.{measure.stats with elapsed=elapsed_time} in
  if String.equal status "complete" then
    Measurement.update_time config stats measure
  else
    measure

let create_minizinc_measure config problem_path result =
  let stats = State.init_global_stats () in
  let measure = Measurement.init stats problem_path (`BoxedOctagon `Integer) 1. in
  let lines = String.split_on_char '\n' result in
  let (numbers, text) = List.partition
    (fun l -> try ignore(int_of_string l); true with Failure _ -> false) lines in
  let numbers = List.map int_of_string numbers in
  let time = (List.hd text) in
  let status = (List.nth text 1) in
  let measure = update_with_optimum measure numbers in
  let measure = update_time config measure time status in
  (measure, status)

let bench_minizinc info config problem_path solver model dzn_file =
  let mzn_command = "minizinc " ^ (minizinc_options config solver) ^ " " ^ model ^ " " ^ dzn_file in
  (* Printf.printf "%s\n" mzn_command; *)
  let _ = call_command mzn_command in
  let result = file_to_string output_file in
  let (measure, status) = create_minizinc_measure config problem_path result in
  if String.equal status "error" then begin
    Measurement.print_exception problem_path "MiniZinc error";
    Measurement.add_erroneous_measure info end
  else begin
    Measurement.print_as_csv config measure;
    Measurement.add_measure info measure
  end

let string_of_list to_string l = List.fold_left (fun s e -> s ^ (to_string e) ^ ", ") "" l

let list_to_mzn name l =
  name ^ " = [" ^
  (string_of_list string_of_int l) ^
  "];\n"

let string_of_2D_list name l =
  name ^ " = [|\n" ^
  (List.fold_left (fun a r -> a ^ (string_of_list string_of_int r) ^ "\n  |") "" l) ^
  "];\n"

let string_of_resources rcpsp =
  let rr = List.map
    (fun r_idx -> List.map (fun j -> List.nth j.resources_usage r_idx) rcpsp.jobs)
    (Tools.range 0 (rcpsp.resources_info.renewable - 1)) in
  string_of_2D_list "rr" rr

let string_of_difference_constraints rcpsp =
  let dc = List.flatten (List.map (fun (p:precedence) ->
      List.map2 (fun w s -> [p.job_index; w; s]) p.weights p.job_successors
    ) rcpsp.precedence_relations) in
  string_of_2D_list "dcons" dc

let make_dzn_data rcpsp =
  (Printf.sprintf "n_res = %d;\n" rcpsp.resources_info.renewable) ^
  (list_to_mzn "rcap" rcpsp.resources) ^
  (Printf.sprintf "n_tasks = %d;\n" rcpsp.jobs_number) ^
  (list_to_mzn "dur" (List.map (fun j -> j.duration) rcpsp.jobs)) ^
  (string_of_resources rcpsp) ^
  (Printf.sprintf "n_dc = %d;\n" (List.fold_left (+) 0 (List.map (fun j -> j.successors) rcpsp.precedence_relations))) ^
  (string_of_difference_constraints rcpsp)

let benchmark_suite_minizinc config solver model =
  Printf.printf "      << %s(%s) >>\n\n" solver model;
  let info = Measurement.empty_info in
  let problems = list_of_problems config in
  Measurement.print_csv_header config;
  let info = List.fold_left (fun info problem_path ->
      let rcpsp = make_rcpsp config problem_path in
      let data = make_dzn_data rcpsp in
      create_dzn_file data;
      bench_minizinc info config problem_path solver model dzn_file
    ) info problems in
  Measurement.print_bench_results model info

(* Flat MiniZinc model: model generated from the Rcpsp_model structure where data is written as constraints.
   This model does not contain global constraint. *)

let mzn_of_bconstraint c = "constraint " ^ string_of_bconstraint c ^ ";\n"

let mzn_of_reified (b, conjunction) =
  let (first, tail) = (List.hd conjunction, List.tl conjunction) in
  "constraint " ^ b ^
  " <-> (" ^ (string_of_bconstraint first) ^
  (List.fold_left (fun a c -> a ^ " /\\ " ^ (string_of_bconstraint c)) "" tail) ^
  ");\n"

let make_mzn_model model search_options =
  (List.fold_left (fun a v -> a ^ "var bool: " ^ v ^ ";\n") "" model.box_vars) ^
  (List.fold_left (fun a v -> a ^ "var int: " ^ v ^ ";\n") "" model.octagonal_vars) ^
  (List.fold_left (fun a c -> a ^ (mzn_of_bconstraint c)) "" model.constraints) ^
  (List.fold_left (fun a r -> a ^ (mzn_of_reified r)) "" model.reified_bconstraints) ^
  (Printf.sprintf "output [show(%s), \"\\n\"];\n" model.makespan) ^
  (Printf.sprintf "solve::int_search([%s], %s) minimize %s;\n"
    (string_of_list (fun x -> x) model.octagonal_vars) search_options model.makespan)

let bench_flat_rcpsp config solver search_options =
  let name = solver ^ "(" ^ search_options ^ ")" in
  Printf.printf "      << %s >>\n\n" name;
  let info = Measurement.empty_info in
  let problems = list_of_problems config in
  Measurement.print_csv_header config;
  let info = List.fold_left (fun info problem_path ->
      let model = Rcpsp_model.create_rcpsp (make_rcpsp config problem_path) in
      let mzn_model = make_mzn_model model search_options in
      create_mzn_file mzn_model;
      bench_minizinc info config problem_path solver mzn_file ""
    ) info problems in
  Measurement.print_bench_results name info