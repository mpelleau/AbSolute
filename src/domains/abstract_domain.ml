(** Signature of abstract domain.
    It differs from `Adcp_sig` because we suppose here that the constraints are encapsulated in the abstract element. *)

type kleene = False | True | Unknown

let rec and_kleene x y =
  match x, y with
  | False, _ -> False
  | True, True -> True
  | True, Unknown -> Unknown
  | Unknown, Unknown -> Unknown
  | _ -> and_kleene y x

(* `conjunction` is the result of the entailment of a conjunction in a reified context.
   It returns the entailment status of the conjunction, with an optional index representing the only `unknown` value, if any.
   See `box_reified` for an example. *)
let and_reified conjunction =
  let (n, t, f, u) =
    List.fold_left (fun (n, t, f, u) -> function
      | True -> (n+1, t+1, f, u)
      | False -> (n+1, t, f+1, u)
      | Unknown -> (n+1, t, f, n::u)) (0,0,0,[]) conjunction in
  if f > 0 then (False, None)
  else if t = n then (True, None)
  else if (List.length u) = 1 then (Unknown, Some(List.hd u))
  else (Unknown, None)

(* This exception is sent when a constraint is passed to an abstract domain that cannot represent this constraint. *)
exception Wrong_modelling of string

module type Abstract_domain =
sig
  (** The module of the bound handled by this abstract domain. *)
  module B: Bound_sig.BOUND

  (** The type of the abstract domain. *)
  type t

  (** Project the lower and upper bounds of a single variable. *)
  val project_one: t -> Csp.var -> (B.t * B.t)

  (** Project the lower and upper bounds of all the variables in `vars`. *)
  val project: t -> Csp.var list -> (Csp.var * (B.t * B.t)) list

  (** Closure of the abstract domain: it tries to remove as much inconsistent values as possible from the abstract element according to the constraints encapsulated. *)
  val closure: t -> t

  (** Weak incremental closure add the constraint into the abstract domain.
      This operation is in constant time and must not perform any closure algorithm.
      It can however raise `Bot_found` if the constraint is detected disentailed in constant time. *)
  val weak_incremental_closure: t -> Csp.bconstraint -> t

  (** Divide the abstract element into sub-elements.
      For exhaustiveness, the union of `split t` should be equal to `t`. *)
  val split: t -> t list

  (** The volume is crucial to get information on the current state of the abstract element:
        - `volume t = 0` means that the current abstract element is failed.
        - `volume t = 1` (on integers) means that the current assignment is satisfiable (note that `1` is exactly representable in a floating point number).
        - On float and rational, the notion of "satisfiability" depends on the expected precision of the abstract element. *)
  val volume: t -> float

  (** An element belongs to one category: failed, satisfiable and unknown.
      Note that this function cannot be recovered from `volume` because `state_decomposition` can return satisfiable even if `volume t > 1` on Z or a given precision on F and Q. *)
  val state_decomposition: t -> kleene
end
