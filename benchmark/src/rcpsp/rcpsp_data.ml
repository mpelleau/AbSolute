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

let ignore_lines file n =
  for _=1 to n do
    ignore(bscanf file "%[^\n]\n" (fun _ -> ()))
  done

let read_trailing_int_list file n =
  let numbers = List.map (fun _ -> bscanf file " %d " (fun x->x)) (Tools.range 1 n) in
  numbers
