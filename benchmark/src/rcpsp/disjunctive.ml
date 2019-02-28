(* This file provides a decomposition of the disjunctive constraint. *)
open Rcpsp_data
open Rcpsp_model
open Csp

(* s1 + d1 <= s2 \/ s2 + d2 <= s1 *)
let non_overlap_constraint j1 j2 =
  let s1 = j1.job_index in
  let s2 = j2.job_index in
  (* Rewritten as octagonal constraints: s1 - s2 <= -d1 \/ s2 - s1 <= -d2 *)
  let c1 = Cmp (LEQ, Binary (SUB, start_job s1, start_job s2), mduration_of j1) in
  let c2 = Cmp (LEQ, Binary (SUB, start_job s2, start_job s1), mduration_of j2) in
  let c = Or (c1, c2) in
  c

(* Ensure that `jobs` are never scheduled at the same time (they do not overlap). *)
let disjunctive jobs =
  for_all_pairs jobs (fun j1 j2 ->
    if j1.job_index < j2.job_index then
      [non_overlap_constraint j1 j2]
    else
      []
  )

let disjunctive_constraints rcpsp =
  List.flatten (List.mapi (fun ir r ->
    if r = 1 then
      disjunctive (List.filter (fun job -> (List.nth job.resources_usage ir) = 1) rcpsp.jobs)
    else
      []
  ) rcpsp.resources)
