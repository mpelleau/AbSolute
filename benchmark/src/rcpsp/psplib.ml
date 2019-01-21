open Sm_format
open Csp

(* Nomenclature of the variables. *)
let start_job i = "start_j" ^ (string_of_int i)
let end_job i = "end_j" ^ (string_of_int i)
let duration_job i = "duration_j" ^ (string_of_int i)

(* Constraint model of the RCPSP. *)
let add_jobs_vars rcpsp csp =
  let add_var name_of csp (job:job) =
    Csp.add_int_var csp
      (name_of job.job_index)
      Bound_rat.zero
      (Bound_rat.of_int rcpsp.horizon) in
  let csp = List.fold_left (add_var start_job) csp rcpsp.jobs in
  let csp = List.fold_left (add_var end_job) csp rcpsp.jobs in
  List.fold_left (add_var duration_job) csp rcpsp.jobs

let add_jobs_constraints rcpsp csp =
  let add_constraint csp (job: job) =
    let i = job.job_index in
    (* end - start = duration *)
    let c = Cmp (EQ, Binary (SUB, Var (end_job i), Var (start_job i)), Var (duration_job i)) in
    {csp with constraints = c::csp.constraints} in
  List.fold_left add_constraint csp rcpsp.jobs

let add_temporal_constraints rcpsp csp =
  (* end_i <= start_j*)
  let precedence_constraint i j = Cmp (LEQ, Var (end_job i), Var (start_job j)) in
  let add_precedence_constraints csp (precedence:precedence) =
    let prec_cons = List.map (precedence_constraint precedence.job_index) precedence.job_successors in
    {csp with constraints = prec_cons@csp.constraints} in
  List.fold_left add_precedence_constraints csp rcpsp.precedence_relations

let add_variables rcpsp =
  Csp.empty |>
  add_jobs_vars rcpsp |>
  add_jobs_constraints rcpsp |>
  add_temporal_constraints rcpsp

(* Precondition: Sanity checks on the file path are supposed to be already done, otherwise it can throw I/O related exceptions.
The files from PSPlib are also supposed to be well-formatted. *)
let psp_to_absolute (problem_path: string) : Csp.prog =
  let rcpsp = read_sm_file problem_path in
  add_variables rcpsp
