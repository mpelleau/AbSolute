(* domain to represent equalities between variables *)
open Signature

(* open Consistency *)
open Tools

module Make (A : Domain) = struct
  (* association from a variable to a set of equal variables. equalitites
     between two variables v1 v2 (where v1 < v2 wrt lexicograpbic order) are
     encoded symbolically. v1 is the representative of all variables equal to
     it *)
  type t = VarSet.t VarMap.t

  type internal_constr = Constraint.comparison

  let print fmt : t -> unit =
    Format.fprintf fmt "%a" (VarMap.print (VarSet.print Format.pp_print_string))

  (* returns the representative of a given variable (can be itself) *)
  let get_representative (eqs : t) (v : string) : string =
    let exception Found of string in
    try
      VarMap.iter
        (fun v' s -> if v' = v || VarSet.mem v s then raise (Found v'))
        eqs ;
      raise Not_found
    with Found v' -> v'

  (* merge two equality classes *)
  let union_class (equalities : t) v1 v2 : t =
    let class1 = VarMap.find v1 equalities in
    let class2 = VarMap.find v2 equalities in
    let union = VarSet.union class1 class2 in
    if v1 <= v2 then VarMap.remove v2 equalities |> VarMap.add v1 union
    else VarMap.remove v1 equalities |> VarMap.add v2 union

  (* registers the equality between v1 and v2. Both must belong to the map
     otherwise a not_foud exception is raised *)
  let add_eq eqs v1 v2 =
    if v1 = v2 then eqs
    else union_class eqs (get_representative eqs v1) (get_representative eqs v2)

  let union (eq1 : t) (eq2 : t) : t =
    VarMap.fold (fun v1 -> VarSet.fold (fun v2 acc -> add_eq acc v1 v2)) eq1 eq2

  let meet (eq1 : t) (eq2 : t) : t =
    VarMap.fold (fun v1 -> VarSet.fold (fun v2 acc -> add_eq acc v1 v2)) eq1 eq2

  let empty = VarMap.empty

  let add_var (a : t) (v : string) = VarMap.add v (VarSet.singleton v) a

  (* removes a variable from the base domain and all its occurences in the
     equalities table *)
  let rm_var (a : t) (v : string) : t =
    let alias = VarMap.remove v a in
    VarMap.map (VarSet.remove v) alias

  (* elem is not used but required by the interface. disabling the unused
     parameter warning to make dune happy *)
  let[@warning "-27"] internalize ?elem = Fun.id

  (* conversion of an element to a set of constraint *)
  let to_constraint a =
    let cell_to_constr v s acc =
      VarSet.fold
        (fun v' acc ->
          if v' = v then acc
          else
            let open Constraint.Operators in
            let eq = Expr.Var v' = Expr.Var v in
            match acc with Some acc -> Some (acc && eq) | None -> Some eq )
        s acc
    in
    VarMap.fold cell_to_constr a None

  (* only equalities are representable *)
  let is_representable c =
    let open Expr in
    match c with
    | Var _, Constraint.EQ, Var _ -> Kleene.True
    | _ -> Kleene.False

  let filter a c =
    let open Expr in
    match c with
    | Var v1, Constraint.EQ, Var v2 ->
        let equalities = add_eq a v1 v2 in
        Consistency.Filtered ((equalities, c), true)
    | _ -> Consistency.Filtered ((a, c), false)

  let spawn _a = failwith "can not spawn"

  let diff = None

  let is_abstraction a (i : Instance.t) : bool =
    try
      VarMap.iter
        (fun v s ->
          if
            not
              (VarSet.for_all (fun v' -> VarMap.find v' i = VarMap.find v i) s)
          then raise Exit )
        a ;
      true
    with Exit -> false

  let forward_eval _ _ = failwith "forward_eval in alias analysis"
end
