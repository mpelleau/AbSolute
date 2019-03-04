open Scanf
open Rcpsp_data

let read_field_value file =
  let s = bscanf file "%[^\n]\n" (fun x -> x) in
  String.trim (List.nth (String.split_on_char ':' s) 1)

let read_int_field_value file =
  let value = read_field_value file in
  int_of_string value

let read_resource_field_value file =
  let value = read_field_value file in
  let value = String.trim (List.nth (String.split_on_char ' ' value) 0) in
  int_of_string value

let read_resources_info file =
  ignore_lines file 1;
  let renewable = read_resource_field_value file in
  let nonrenewable = read_resource_field_value file in
  let doubly_constrained = read_resource_field_value file in
  {renewable=renewable; nonrenewable=nonrenewable; doubly_constrained=doubly_constrained}

let read_project_info file =
  bscanf file " %d %d %d %d %d %d\n" (fun a b c d e f -> {
    project_idx=a;
    jobs=b;
    rel_date=c;
    due_date=d;
    tard_cost=e;
    mpm_time=f
  })

let read_rcpsp_info file =
  let projects = read_int_field_value file in
  let jobs_number = read_int_field_value file in
  let horizon = read_int_field_value file in
  let resources_info = read_resources_info file in
  ignore_lines file 3;
  let project_info = read_project_info file in
  { projects=projects;
    jobs_number=jobs_number;
    horizon=horizon;
    resources_info=resources_info;
    project_info=project_info;
    precedence_relations=[];
    jobs=[];
    resources=[]
  }

let read_precedence file =
  let prec = bscanf file " %d %d %d " (fun a b c -> {
    job_index=a;
    mode=b;
    successors=c;
    job_successors=[];
    weights=[];
  }) in
  let job_successors = read_trailing_int_list file prec.successors in
  {prec with job_successors=job_successors}

let read_precedence_relations file rcpsp =
  ignore_lines file 3;
  let precedence_relations = List.map (fun _ -> read_precedence file) (Tools.range 1 rcpsp.jobs_number) in
  { rcpsp with precedence_relations=precedence_relations}

let read_job file rcpsp =
  let job = bscanf file " %d %d %d " (fun a b c -> {
    job_index=a;
    mode=b;
    duration=c;
    resources_usage=[]
  }) in
  let job = {job with resources_usage=read_trailing_int_list file (number_of_resources rcpsp)} in
  let precedence_relations = List.map (fun (prec:precedence) ->
    if prec.job_index = job.job_index then
      let durations = List.map (fun _ -> job.duration) (prec.job_successors) in
      {prec with weights=durations}
    else prec
  ) rcpsp.precedence_relations in
  {rcpsp with
    precedence_relations=precedence_relations;
    jobs=rcpsp.jobs@[job];
  }

let read_jobs file rcpsp =
  ignore_lines file 4;
  List.fold_left (fun rcpsp _ -> read_job file rcpsp) rcpsp (Tools.range 1 rcpsp.jobs_number)

let read_resource_availabilities file rcpsp =
  ignore_lines file 3;
  let resources = read_trailing_int_list file (number_of_resources rcpsp) in
  {rcpsp with resources=resources}

let read_sm file =
  ignore_lines file 4;
  read_rcpsp_info file |>
  read_precedence_relations file |>
  read_jobs file |>
  read_resource_availabilities file

(* see preconditions on `problem_path` at `psplib_to_absolute`. *)
let read_sm_file (problem_path: string) : rcpsp =
  let file = Scanning.open_in problem_path in
  let rcpsp = read_sm file in
  Scanning.close_in file;
  rcpsp
