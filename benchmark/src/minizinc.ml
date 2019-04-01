open Printf
open System
open Factory
open Rcpsp
open Rcpsp_data
open Rcpsp_model
open Csp

let dzn_file = "tmp.dzn"
let mzn_file = "tmp.mzn"

let create_file data name =
  let oc = open_out name in
  fprintf oc "%s\n" data;
  close_out oc

let create_dzn_file data = create_file data dzn_file
let create_mzn_file data = create_file data mzn_file

let bench_minizinc model dzn_file =
  let mzn_command = "minizinc " ^ model ^ " " ^ dzn_file in
  Printf.printf "%s\n" mzn_command;
  let status = call_command mzn_command in
  Printf.printf "status = %d\n" status

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

let benchmark_suite_minizinc config model =
  let problems = list_of_problems config in
  List.iter (fun problem_path ->
    let rcpsp = make_rcpsp config problem_path in
    let data = make_dzn_data rcpsp in
    create_dzn_file data;
    bench_minizinc model dzn_file
  ) problems

let mzn_of_bconstraint c = "constraint " ^ string_of_bconstraint c ^ ";\n"

let mzn_of_reified (b, conjunction) =
  let (first, tail) = (List.hd conjunction, List.tl conjunction) in
  "constraint " ^ b ^
  " <-> (" ^ (string_of_bconstraint first) ^
  (List.fold_left (fun a c -> a ^ " /\\ " ^ (string_of_bconstraint c)) "" tail) ^
  ");\n"

let make_mzn_model model =
  (List.fold_left (fun a v -> a ^ "var bool: " ^ v ^ ";\n") "" model.box_vars) ^
  (List.fold_left (fun a v -> a ^ "var int: " ^ v ^ ";\n") "" model.octagonal_vars) ^
  (List.fold_left (fun a c -> a ^ (mzn_of_bconstraint c)) "" model.constraints) ^
  (List.fold_left (fun a r -> a ^ (mzn_of_reified r)) "" model.reified_bconstraints) ^
  (Printf.sprintf "solve::int_search([%s], first_fail, indomain_min, complete) minimize %s;\n"
    (string_of_list (fun x -> x) model.octagonal_vars) model.makespan)

let bench_flat_rcpsp config =
  let problems = list_of_problems config in
  List.iter (fun problem_path ->
    let model = Rcpsp_model.create_rcpsp (make_rcpsp config problem_path) in
    let mzn_model = make_mzn_model model in
    create_mzn_file mzn_model;
    bench_minizinc mzn_file ""
  ) problems