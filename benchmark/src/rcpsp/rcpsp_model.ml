(* Constraint model of the RCPSP. *)
open Rcpsp_data
open Csp
open Box_reified

(* I. Utility functions to create the model of the RCPSP. *)

let for_all_pairs jobs f =
  List.flatten (List.map (fun j1 ->
    List.flatten (List.map (fun j2 ->
      f j1 j2
    ) jobs)
  ) jobs)

let for_all_distinct_pairs jobs f =
  for_all_pairs jobs (fun j1 j2 ->
    if j1.job_index <> j2.job_index then
      [f j1 j2]
    else
      [])

(* II. Creation of the variables for the RCPSP. *)

(* Name factory of the variables. *)
let start_job_name i = "start_j" ^ (string_of_int i)
let start_job i = Var (start_job_name i)
let start_job' job = Var (start_job_name job.job_index)

let constant_of i = Cst (Bound_rat.of_int i, Int)
let duration_of job = constant_of job.duration
let mduration_of job = constant_of (-job.duration)

let makespan_name rcpsp = start_job_name rcpsp.jobs_number

(* These variables are generated for the "task decomposition" of the cumulative constraint.
   We have `job_1_runs_when_2_starts = 1` if the job `1` starts when the job `2` is running.
   Importantly: these variables are shared across all cumulatives. *)
let job_start_when_name j1 j2 =
  "job_" ^ (string_of_int j1.job_index) ^ "_runs_when_" ^ (string_of_int j2.job_index) ^ "_starts"
let job_start_when j1 j2 = Var (job_start_when_name j1 j2)

(* The octagonal variables are the starting dates of the jobs. *)
let octagonal_variables rcpsp =
  let name_of job = start_job_name job.job_index in
  List.map name_of rcpsp.jobs

(* Create boolean variables modelling the overlapping of two tasks. *)
let overlap_boolean_variables rcpsp = for_all_distinct_pairs rcpsp.jobs job_start_when_name

(* III. Constraints of the RCPSP. *)

(* Domain of the variables *)

let var_domain_constraints rcpsp =
  let dom u v = [
    (Var v, GEQ, Cst (Bound_rat.zero, Int));
    (Var v, LEQ, Cst (Bound_rat.of_int u, Int))
  ] in
  let ov = octagonal_variables rcpsp in
  List.flatten (
    [(dom 0 (List.hd ov))]@
    (List.map (dom rcpsp.horizon) (List.tl ov))@
    (List.map (dom 1) (overlap_boolean_variables rcpsp))@
    [(dom rcpsp.horizon (makespan_name rcpsp))])

(* Generalized temporal constraints: ensure a precedence (with a possible timelag) between the tasks. *)
let temporal_constraints rcpsp =
  (* s1 + d1 <= s2 rewritten to s1 - s2 <= -d1 *)
  let precedence_constraint (prec:precedence) (j,w) =
    let i = prec.job_index in
    let m_weight_i = constant_of (-w) in
    (Binary (SUB, start_job i, start_job j), LEQ, m_weight_i) in
  let all_successors (precedence:precedence) =
    List.map (precedence_constraint precedence)
      (List.map2 (fun x y -> (x,y)) precedence.job_successors precedence.weights) in
  List.flatten (List.map all_successors rcpsp.precedence_relations)

(* let rewrite_and_create c =
  match RewriterZ.rewrite c with
  | [] -> failwith "impossible to rewrite the constraint into an octagonal version."
  | x -> x *)

(* job_1_runs_when_2_starts = 1 <=> s[1] <= s[2] /\ s[2] < s[1] + d[1] *)
let overlap_reified_constraints rcpsp =
  for_all_distinct_pairs rcpsp.jobs (fun j1 j2 ->
    let c1 = (start_job' j1, LEQ, start_job' j2) in
    let c2 = (Binary (SUB, start_job' j2, start_job' j1), LT, duration_of j1) in
    (job_start_when_name j1 j2, [c1; c2]))

(* let overlap_reified_octagonal rcpsp =
  let overlap_constraints = overlap_reified_constraints rcpsp in
  List.map (fun (b, conjunction) -> (b, List.flatten (List.map rewrite_and_create conjunction))) overlap_constraints
 *)

(* Tasks decomposition of cumulative:
      forall j1, capacity_ri >= r[j1] + sum (job_2_runs_when_1_starts * r[j2]) where j2 <> j1 *)
let cumulative_constraint rcpsp ri =
  (* We retrieve the jobs that use the resource at index `ri`. *)
  let jobs = List.filter (fun job -> (List.nth job.resources_usage ri) > 0) rcpsp.jobs in
  List.map (fun j1 ->
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
    (capacity, GEQ, Binary (ADD, resource_j1, sum_resource_j2))
  ) jobs

let all_cumulatives rcpsp =
  let resource_indexes = Tools.range 0 ((List.length rcpsp.resources) - 1) in
  List.flatten (List.map (cumulative_constraint rcpsp) resource_indexes)

(* The octagonal variables are the starting dates.
   The box variables include the octagonal variables, and the boolean overlap variables. *)
type rcpsp_model = {
  makespan: var;
  box_vars: var list;
  octagonal_vars: var list;
  constraints: bconstraint list;
  (* reified_octagonal: reified_octagonal list; *)
  reified_bconstraints: box_reified_constraint list;
}

let create_rcpsp rcpsp = {
  makespan=(makespan_name rcpsp);
  box_vars=overlap_boolean_variables rcpsp;
  octagonal_vars=octagonal_variables rcpsp;
  constraints=(var_domain_constraints rcpsp)@(all_cumulatives rcpsp)@(temporal_constraints rcpsp);
  (* reified_octagonal=(overlap_reified_octagonal rcpsp); *)
  reified_bconstraints=(overlap_reified_constraints rcpsp);
}
