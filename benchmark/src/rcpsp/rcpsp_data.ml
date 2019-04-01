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
  job_successors: int list;
  (* for all successor job_j: job_i + weights_ij <= job_j *)
  weights: int list;
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

(* Parsing/utility functions that are common across formats (Patterson, SM, ProGen/max).  *)

let ignore_lines file n =
  for _=1 to n do
    ignore(bscanf file "%[^\n]\n" (fun _ -> ()))
  done

let read_trailing_int_list file n =
  let numbers = List.map (fun _ -> bscanf file " %d " (fun x->x)) (Tools.range 1 n) in
  numbers

let make_dumb_project_info jobs_number = {
  project_idx=0;
  jobs=jobs_number-2;
  rel_date=0;
  due_date=0;
  tard_cost=0;
  mpm_time=0;
}

let number_of_resources rcpsp =
  let r = rcpsp.resources_info in
  r.renewable + r.nonrenewable + r.doubly_constrained

let compute_horizon rcpsp =
  let horizon = List.fold_left (fun a j ->
    let weights = List.flatten (List.map (fun (p:precedence) ->
      if p.job_index = j.job_index then p.weights else []) rcpsp.precedence_relations) in
    let max_dur = List.fold_left max j.duration weights in
    a + max_dur
    ) 0 rcpsp.jobs in
  {rcpsp with horizon = horizon}
