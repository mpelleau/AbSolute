open Sm_format
open Csp

(* Nomenclature of the variables. *)
let start_job i = "start_j" ^ (string_of_int i)
let end_job i = "end_j" ^ (string_of_int i)

let duration_of job = Cst (Bound_rat.of_int job.duration, Int)
let mduration_of job = Cst (Bound_rat.of_int (-job.duration), Int)

(* Constraint model of the RCPSP. *)
let add_jobs_vars rcpsp csp =
  let add_var name_of csp (job:job) =
    Csp.add_int_var csp
      (name_of job.job_index)
      Bound_rat.zero
      (Bound_rat.of_int rcpsp.horizon) in
  let csp = List.fold_left (add_var start_job) csp rcpsp.jobs in
  let csp = List.fold_left (add_var end_job) csp rcpsp.jobs in
  csp

let add_jobs_constraints rcpsp csp =
  let add_constraint csp (job: job) =
    let i = job.job_index in
    (* end - start = duration *)
    let c = Cmp (EQ, Binary (SUB, Var (end_job i), Var (start_job i)), duration_of job) in
    {csp with constraints = c::csp.constraints} in
  List.fold_left add_constraint csp rcpsp.jobs

let add_temporal_constraints rcpsp csp =
  (* end_i <= start_j*)
  let precedence_constraint i j = Cmp (LEQ, Var (end_job i), Var (start_job j)) in
  let add_precedence_constraints csp (precedence:precedence) =
    let prec_cons = List.map (precedence_constraint precedence.job_index) precedence.job_successors in
    {csp with constraints = prec_cons@csp.constraints} in
  List.fold_left add_precedence_constraints csp rcpsp.precedence_relations

(* s1 + d1 <= s2 \/ s2 + d2 <= s1 *)
let non_overlap_constraint j1 j2 =
  let s1 = j1.job_index in
  let s2 = j2.job_index in
  (* Rewritten as octagonal constraints: s1 - s2 <= -d1 \/ s2 - s1 <= -d2 *)
  let c1 = Cmp (LEQ, Binary (SUB, Var (start_job s1), Var (start_job s2)), mduration_of j1) in
  let c2 = Cmp (LEQ, Binary (SUB, Var (start_job s2), Var (start_job s1)), mduration_of j2) in
  let c = Or (c1, c2) in
  c

(* Ensure that the `jobs` are never scheduled at the same time (they do not overlap). *)
let disjunctive jobs =
  List.flatten (List.map (fun j1 ->
    List.flatten (List.map (fun j2 ->
      if j1.job_index < j2.job_index then
        [non_overlap_constraint j1 j2]
      else
        []
    ) jobs)
  ) jobs)

let add_disjunctive_constraints rcpsp csp =
  let constraints =
    List.mapi (fun ir r ->
      if r = 1 then
        disjunctive (List.filter (fun job -> (List.nth job.resources_usage ir) = 1) rcpsp.jobs)
      else
        []
    ) rcpsp.resources in
  List.fold_left (fun csp c -> {csp with constraints = c@csp.constraints}) csp constraints

let add_variables rcpsp =
  Csp.empty |>
  add_jobs_vars rcpsp |>
  add_jobs_constraints rcpsp |>
  add_temporal_constraints rcpsp |>
  add_disjunctive_constraints rcpsp

(* Precondition: Sanity checks on the file path are supposed to be already done, otherwise it can throw I/O related exceptions.
The files from PSPlib are also supposed to be well-formatted. *)
let psp_to_absolute (problem_path: string) : Csp.prog =
  let rcpsp = read_sm_file problem_path in
  add_variables rcpsp
