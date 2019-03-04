open Scanf
open Rcpsp_data

(* NOTE: We index the jobs from 1 to N (to keep it uniform among formats).
   In this file format, there are initially indexed from 0. *)

let read_resources_info file =
  bscanf file " %d %d %d" (fun a b c ->
    {renewable=a; nonrenewable=b; doubly_constrained=c})

let read_pro_gen_info file =
  let jobs_number = bscanf file "%d " (fun x->x+2) in
  let resources_info = read_resources_info file in
  ignore_lines file 1;
  { projects=0;
    jobs_number=jobs_number;
    horizon=0;
    resources_info=resources_info;
    project_info=make_dumb_project_info jobs_number;
    precedence_relations=[];
    jobs=[];
    resources=[]
  }

let read_trailing_weights file n =
  List.map (fun _ -> bscanf file " [%d]" (fun x->x)) (Tools.range 1 n)

let read_job file rcpsp _ =
  let job_index = bscanf file "%d " (fun a -> a+1) in
  let mode = bscanf file " %d " (fun a -> a) in (* Not sure it's the mode; always at "1" in the data set. *)
  let successor_number = bscanf file " %d" (fun a -> a) in
  let job_successors = List.map (fun j -> j + 1) (read_trailing_int_list file successor_number) in
  let weights = read_trailing_weights file successor_number in
  ignore_lines file 1;
  let job = {
    job_index=job_index;
    mode=mode;
    duration=0;
    resources_usage=[];
  } in
  let precedence = {
    job_index=job_index;
    mode=mode;
    successors=successor_number;
    job_successors=job_successors;
    weights=weights;
  } in
  { rcpsp with
      jobs=rcpsp.jobs@[job];
      precedence_relations=rcpsp.precedence_relations@[precedence] }

let read_jobs file rcpsp =
  List.fold_left (read_job file) rcpsp (Tools.range 1 rcpsp.jobs_number)

let read_job_info file rcpsp _ =
  let job_index = bscanf file "%d " (fun a -> a+1) in
  let _ = bscanf file " %d " (fun a -> a) in (* Not sure it's the mode; always at "1" in the data set. *)
  let duration = bscanf file " %d " (fun a -> a) in
  let resources_usage = read_trailing_int_list file rcpsp.resources_info.renewable in
  let jobs = List.map (fun job ->
    if job.job_index = job_index then
      {job with duration=duration; resources_usage=resources_usage}
    else job) rcpsp.jobs in
  { rcpsp with jobs=jobs }

let read_resources_capacity file rcpsp =
  {rcpsp with
    resources = read_trailing_int_list file rcpsp.resources_info.renewable}

let read_duration_and_resources file rcpsp =
  let rcpsp = List.fold_left (read_job_info file) rcpsp (Tools.range 1 rcpsp.jobs_number) in
  read_resources_capacity file rcpsp

let read_pro_gen file =
  read_pro_gen_info file |>
  read_jobs file |>
  read_duration_and_resources file |>
  compute_horizon

(* see preconditions on `problem_path` at `psplib_to_absolute`. *)
let read_pro_gen_file (problem_path: string) : rcpsp =
  let file = Scanning.open_in problem_path in
  let rcpsp = read_pro_gen file in
  Scanning.close_in file;
  rcpsp
