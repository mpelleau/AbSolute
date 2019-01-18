open Scanf

(* The information on the number and kind of resources available. *)
type resources_info = {
  renewable: int;
  nonrenewable: int;
  doubly_constrained: int;
}

(* The information for one project. *)
type project_info = {
  project_idx: int;
  jobs: int; (* without source and sink. *)
  rel_date: int;
  due_date: int;
  tard_cost: int;
  mpm_time: int
}

type precedence = {
  job_index: int;
  mode: int;
  successors: int;
  job_successors: int list
}

type job = {
  job_index: int;
  mode: int;
  duration: int;
  resources_usage: int list
}

type rcpsp = {
  projects: int;
  jobs_number: int; (* including dummy source and sink. *)
  horizon: int;
  resources_info: resources_info;
  project_info: project_info;
  precedence_relations: precedence list;
  jobs: job list;
  resources: int list
}

let number_of_resources rcpsp =
  let r = rcpsp.resources_info in
  r.renewable + r.nonrenewable + r.doubly_constrained

let ignore_lines file n =
  for _=1 to n do
    ignore(bscanf file "%[^\n]\n" (fun _ -> ()))
  done

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
  Scanf.bscanf file " %d %d %d %d %d %d\n" (fun a b c d e f -> {
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

let read_trailing_int_list file n =
  let numbers = List.map (fun _ -> bscanf file " %d " (fun x->x)) (Tools.range 1 n) in
  numbers

let read_precedence file =
  let prec = bscanf file " %d %d %d " (fun a b c -> {
    job_index=a;
    mode=b;
    successors=c;
    job_successors=[]
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
  let resources_usage = read_trailing_int_list file (number_of_resources rcpsp) in
  {job with resources_usage=resources_usage}

let read_jobs file rcpsp =
  ignore_lines file 4;
  let jobs = List.map (fun _ -> read_job file rcpsp) (Tools.range 1 rcpsp.jobs_number) in
  { rcpsp with jobs=jobs}

let read_resource_availabilities file rcpsp =
  ignore_lines file 3;
  let resources = read_trailing_int_list file (number_of_resources rcpsp) in
  {rcpsp with resources=resources}

let read_rcpsp file =
  ignore_lines file 4;
  read_rcpsp_info file |>
  read_precedence_relations file |>
  read_jobs file |>
  read_resource_availabilities file

(* Precondition: Sanity checks on the file path are supposed to be already done, otherwise it can throw I/O related exceptions.
The files from PSPlib are also supposed to be well-formatted. *)
let psp_to_absolute (problem_path: string) : Csp.prog =
  let file = Scanf.Scanning.open_in problem_path in
  let _ = read_rcpsp file in
  Scanf.Scanning.close_in file;
  Csp.empty
