open Rcpsp_data
open Csp

(* Nomenclature of the variables. *)
let start_job_name i = "start_j" ^ (string_of_int i)
let end_job_name i = "end_j" ^ (string_of_int i)
let start_job i = Var (start_job_name i)
let end_job i = Var (end_job_name i)
let start_job' job = Var (start_job_name job.job_index)
let end_job' job = Var (end_job_name job.job_index)

let constant_of i = Cst (Bound_rat.of_int i, Int)
let duration_of job = constant_of job.duration
let mduration_of job = constant_of (-job.duration)

(* Constraint model of the RCPSP. *)
let add_jobs_vars rcpsp csp =
  let add_var name_of csp (job:job) =
    Csp.add_int_var csp
      (name_of job.job_index)
      Bound_rat.zero
      (Bound_rat.of_int rcpsp.horizon) in
  let csp = List.fold_left (add_var start_job_name) csp rcpsp.jobs in
  let csp = List.fold_left (add_var end_job_name) csp rcpsp.jobs in
  csp

let add_jobs_constraints rcpsp csp =
  let add_constraint csp (job: job) =
    let i = job.job_index in
    (* end - start = duration *)
    let c = Cmp (EQ, Binary (SUB, end_job i, start_job i), duration_of job) in
    {csp with constraints = c::csp.constraints} in
  List.fold_left add_constraint csp rcpsp.jobs

let add_temporal_constraints rcpsp csp =
  (* end_i <= start_j*)
  let precedence_constraint i j = Cmp (LEQ, end_job i, start_job j) in
  let add_precedence_constraints csp (precedence:precedence) =
    let prec_cons = List.map (precedence_constraint precedence.job_index) precedence.job_successors in
    {csp with constraints = prec_cons@csp.constraints} in
  List.fold_left add_precedence_constraints csp rcpsp.precedence_relations

(* s1 + d1 <= s2 \/ s2 + d2 <= s1 *)
let non_overlap_constraint j1 j2 =
  let s1 = j1.job_index in
  let s2 = j2.job_index in
  (* Rewritten as octagonal constraints: s1 - s2 <= -d1 \/ s2 - s1 <= -d2 *)
  let c1 = Cmp (LEQ, Binary (SUB, start_job s1, start_job s2), mduration_of j1) in
  let c2 = Cmp (LEQ, Binary (SUB, start_job s2, start_job s1), mduration_of j2) in
  let c = Or (c1, c2) in
  c

let for_all_pairs jobs f =
  List.flatten (List.map (fun j1 ->
    List.flatten (List.map (fun j2 ->
      f j1 j2
    ) jobs)
  ) jobs)

(* Ensure that the `jobs` are never scheduled at the same time (they do not overlap). *)
let disjunctive jobs =
  for_all_pairs jobs (fun j1 j2 ->
    if j1.job_index < j2.job_index then
      [non_overlap_constraint j1 j2]
    else
      []
  )

let add_disjunctive_constraints rcpsp csp =
  let constraints =
    List.flatten (List.mapi (fun ir r ->
      if r = 1 then
        disjunctive (List.filter (fun job -> (List.nth job.resources_usage ir) = 1) rcpsp.jobs)
      else
        []
    ) rcpsp.resources) in
  {csp with constraints = csp.constraints@constraints}

(* These variables are generated for the "task decomposition" of the cumulative constraint.
   We have `job_1_runs_when_2_starts = 1` if the job `1` starts when the job `2` is running.
   Importantly: these variables are shared across all cumulatives. *)
let job_start_when_name j1 j2 =
  "job_" ^ (string_of_int j1.job_index) ^ "_runs_when_" ^ (string_of_int j2.job_index) ^ "_starts"

let job_start_when j1 j2 = Var (job_start_when_name j1 j2)

let for_all_distinct_pairs jobs f =
  for_all_pairs jobs (fun j1 j2 ->
    if j1.job_index <> j2.job_index then
      [f j1 j2]
    else
      [])

let add_shared_cumulative_variables rcpsp csp =
  let vars = for_all_distinct_pairs rcpsp.jobs job_start_when_name in
  List.fold_left (fun csp v ->
    Csp.add_int_var csp v Bound_rat.zero Bound_rat.one) csp vars

(* We have the following equivalence:
     job_1_runs_when_2_starts <=> s[1] <= s[2] /\ s[2] < s[1] + d[1]
   which is equivalent to:
     (job_1_runs_when_2_starts = 1 /\ s[1] <= s[2] /\ s[2] < s[1] + d[1]) \/
     (job_1_runs_when_2_starts = 0 /\ s[1] > s[2]) \/
     (job_1_runs_when_2_starts = 0 /\ s[2] >= s[1] + d[1])) *)
let add_shared_cumulative_constraints rcpsp csp =
  let overlap_cons = for_all_distinct_pairs rcpsp.jobs (fun j1 j2 ->
    let btrue = Cmp (EQ, job_start_when j1 j2, constant_of 1) in
    let bfalse = Cmp (EQ, job_start_when j1 j2, constant_of 0) in
    let c1 = Cmp (LEQ, start_job' j1, start_job' j2) in
    let c2 = Cmp (LT, start_job' j2, Binary (ADD, start_job' j1, duration_of j1)) in
    let c3 = Cmp (GT, start_job' j1, start_job' j2) in
    let c4 = Cmp (GEQ, start_job' j2, Binary (ADD, start_job' j1, duration_of j1)) in
    Or (And (And (btrue, c1), c2), Or (And (bfalse, c3), And (bfalse, c4)))
  ) in
  {csp with constraints = csp.constraints@overlap_cons}

(* Tasks decomposition of cumulative:
      forall j1, capacity_ri >= r[j1] + sum (job_2_runs_when_1_starts * r[j2]) where j2 <> j1 *)
let add_cumulative_constraint rcpsp csp ri =
  (* We retrieve the jobs that use the resource at index `ri`. *)
  let jobs = List.filter (fun job -> (List.nth job.resources_usage ri) > 0) rcpsp.jobs in
  let cumulative = List.map (fun j1 ->
    let resources_j2 = List.flatten (List.map (fun j2 ->
      if j1.job_index <> j2.job_index then
        [Binary (MUL, job_start_when j2 j1, constant_of (List.nth j2.resources_usage ri))]
      else
        []
    ) jobs)
    in
    let sum_resource_j2 = List.fold_left (fun sum mul -> Binary (ADD, sum, mul))
      (constant_of 0) resources_j2 in
    let capacity = constant_of (List.nth rcpsp.resources ri) in
    let resource_j1 = constant_of (List.nth j1.resources_usage ri) in
    Cmp (GEQ, capacity, Binary (ADD, resource_j1, sum_resource_j2))
  ) jobs in
  {csp with constraints = csp.constraints@cumulative}

let add_all_cumulatives rcpsp csp =
  List.fold_left (add_cumulative_constraint rcpsp) csp
    (Tools.range 0 ((List.length rcpsp.resources) - 1))

let create_rcpsp rcpsp =
  Csp.empty |>
  add_jobs_vars rcpsp |>
  add_jobs_constraints rcpsp |>
  add_temporal_constraints rcpsp |>
  add_disjunctive_constraints rcpsp |>
  add_shared_cumulative_variables rcpsp |>
  add_shared_cumulative_constraints rcpsp |>
  add_all_cumulatives rcpsp

(* Precondition: Sanity checks on the file path are supposed to be already done, otherwise it can throw I/O related exceptions.
The files from PSPlib are also supposed to be well-formatted. *)
let psp_to_absolute (problem_path: string) : Csp.prog =
  let rcpsp = Sm_format.read_sm_file problem_path in
  create_rcpsp rcpsp

let patterson_to_absolute (problem_path: string) : Csp.prog =
  let rcpsp = Patterson.read_patterson_file problem_path in
  create_rcpsp rcpsp
