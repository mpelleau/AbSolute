open Scanf
open Rcpsp_data

let make_resources_info r = {
  renewable=r;
  nonrenewable=0;
  doubly_constrained=0;
}

let read_resource_availabilities file resources_number =
  let resources = read_trailing_int_list file resources_number in
  resources

let read_rcpsp_info file =
  let (jobs_number, resources_number) = bscanf file " %d %d " (fun a b -> (a,b)) in
  let project_info = make_dumb_project_info jobs_number in
  let resources_info = make_resources_info resources_number in
  let resources = read_resource_availabilities file resources_number in
  { projects=0;
    jobs_number=jobs_number;
    horizon=0;
    resources_info=resources_info;
    project_info=project_info;
    precedence_relations=[];
    jobs=[];
    resources=resources
  }

let read_job file rcpsp job_index =
  let duration = bscanf file " %d " (fun a -> a) in
  let resources_usage = read_trailing_int_list file (List.length rcpsp.resources) in
  let successor_number = bscanf file " %d " (fun a -> a) in
  let job_successors = read_trailing_int_list file successor_number in
  let job = {
    job_index=job_index;
    mode=1;
    duration=duration;
    resources_usage=resources_usage;
  } in
  let precedence = {
    job_index=job_index;
    mode=1;
    successors=successor_number;
    job_successors=job_successors;
    weights=List.map (fun _ -> duration) job_successors;
  } in
  { rcpsp with
      jobs=rcpsp.jobs@[job];
      precedence_relations=rcpsp.precedence_relations@[precedence] }

let read_jobs file rcpsp =
  List.fold_left (read_job file) rcpsp (Tools.range 1 rcpsp.jobs_number)

let read_patterson file =
  read_rcpsp_info file |>
  read_jobs file |>
  compute_horizon

(* see preconditions on `problem_path` at `psplib_to_absolute`. *)
let read_patterson_file (problem_path: string) : rcpsp =
  let file = Scanning.open_in problem_path in
  let rcpsp = read_patterson file in
  Scanning.close_in file;
  rcpsp
