(* Domain to represent equalities between variables, using a union-find
   approach: Each variable is initially alone in its own equivalence class. When
   an equality between two variables is found, their equivalence classes are
   merged together, the smallest variable wrt lex. order is set as the
   representative of the class *)

open Signature
open Tools

type unionfind = VarSet.t VarMap.t

let uf_fold f = VarMap.fold (fun v -> VarSet.fold (f v))

type t =
  {support: (Csp.typ * string * Dom.t) list; equalities: VarSet.t VarMap.t}

type internal_constr = Constraint.comparison

(* elem is not used but required by the interface. disabling the unused
   parameter warning to make dune happy *)
let[@warning "-27"] internalize ?elem = Fun.id

let externalize = Fun.id

let print fmt a =
  Format.fprintf fmt "%a"
    (VarMap.print (VarSet.print Format.pp_print_string))
    a.equalities

(* returns the representative of a given variable (can be itself) *)
let get_representative (a : t) (v : string) : string =
  let exception Found of string in
  try
    VarMap.iter
      (fun v' s -> if v' = v || VarSet.mem v s then raise (Found v'))
      a.equalities ;
    raise Not_found
  with Found v' -> v'

(* merge two equality classes *)
let union_class (a : t) v1 v2 : t =
  let class1 = VarMap.find v1 a.equalities in
  let class2 = VarMap.find v2 a.equalities in
  let union = VarSet.union class1 class2 in
  let equalities =
    if v1 <= v2 then VarMap.remove v2 a.equalities |> VarMap.add v1 union
    else VarMap.remove v1 a.equalities |> VarMap.add v2 union
  in
  {a with equalities}

(* registers the equality between v1 and v2. Both must belong to the map
   otherwise a not_foud exception is raised *)
let add_eq eqs v1 v2 =
  if v1 = v2 then eqs
  else union_class eqs (get_representative eqs v1) (get_representative eqs v2)

(* checks if two variable are equal within a map *)
let are_eq eqs v1 v2 = get_representative eqs v1 = get_representative eqs v2

(* we rebuild a map where we add information present in both equality table.
   both support should be the same. The support computed is the one of eq1 *)
let union (eq1 : t) (eq2 : t) : t =
  uf_fold
    (fun v1 v2 acc -> if are_eq eq2 v1 v2 then add_eq acc v1 v2 else acc)
    eq1.equalities
    {eq1 with equalities= VarMap.map (fun _ -> VarSet.empty) eq1.equalities}

let join (eq1 : t) (eq2 : t) : t * bool = (union eq1 eq2, true)

let join_list = function
  | [] -> raise Bot_found
  | h :: tl -> (List.fold_left union h tl, true)

(* this meet operation can never return None, as no incompatible information can
   be stored in two elements *)
let meet (eq1 : t) (eq2 : t) : t option =
  Some (uf_fold (fun v1 v2 acc -> add_eq acc v1 v2) eq1.equalities eq2)

let empty = {equalities= VarMap.empty; support= []}

let add_var (a : t) ((_, v, _) as decl : Csp.decl) =
  { equalities= VarMap.add v (VarSet.singleton v) a.equalities
  ; support= decl :: a.support }

(* removes a variable from the base domain and all its occurences in the
   equalities table *)
let rm_var (a : t) (v : string) : t =
  let alias = VarMap.remove v a.equalities in
  { equalities= VarMap.map (VarSet.remove v) alias
  ; support= List.filter (fun (_, v', _) -> v <> v') a.support }

(* conversion of an element to a set of constraint. FIXME: might be none *)
let to_constraint (a : t) =
  let cell_to_constr v v' acc =
    if v' = v then acc
    else
      let open Constraint.Operators in
      let eq = Expr.Var v' = Expr.Var v in
      match acc with Some acc -> Some (acc && eq) | None -> Some eq
  in
  match uf_fold cell_to_constr a.equalities None with
  | Some s -> s
  | None -> raise Top_found

(* only equalities are representable *)
let is_representable (c : internal_constr) : Kleene.t =
  let open Expr in
  match c with Var _, Constraint.EQ, Var _ -> Kleene.True | _ -> Kleene.False

let sat (i : Instance.t) (c : internal_constr) : bool =
  Constraint.eval_comparison c i

(* *)
let filter (a : t) (c : internal_constr) =
  let open Expr in
  match c with
  | Var v1, Constraint.EQ, Var v2 ->
      let equalities = add_eq a v1 v2 in
      Consistency.Filtered (equalities, true)
  | _ -> Consistency.Filtered (a, false)

let filter_diff (a : t) (c : internal_constr) =
  let open Expr in
  match c with
  | Var v1, Constraint.EQ, Var v2 ->
      let equalities = add_eq a v1 v2 in
      Consistency.Filtered ((equalities, VarSet.empty), true)
  | _ -> Consistency.Filtered ((a, VarSet.empty), false)

let vars a = a.support

let dom_spawn dom typ =
  let open Dom in
  match (dom, typ) with
  | Finite (l, h), Csp.Real ->
      Q.add l (Q.mul (Q.sub h l) (Q.of_float (Random.float 1.)))
  | Finite (l, h), Csp.Int ->
      let l = Q.ceil l in
      let h = Q.floor h in
      l + Random.int (h - l) |> Q.of_int (* sensible to overflows *)
  | _ -> failwith "non finite domain"

let spawn a : Instance.t =
  VarMap.fold
    (fun v1 s acc ->
      if VarMap.mem v1 acc then acc
      else
        let t, _, dom = List.find (fun (_, v, _) -> v = v1) a.support in
        let value = dom_spawn dom t in
        VarSet.fold (fun v acc -> VarMap.add v value acc) s acc )
    a.equalities VarMap.empty

let is_abstraction a (i : Instance.t) : bool =
  try
    VarMap.iter
      (fun v s ->
        if not (VarSet.for_all (fun v' -> VarMap.find v' i = VarMap.find v i) s)
        then raise Exit )
      a.equalities ;
    true
  with Exit -> false

(* not splittable *)
let[@warning "-27"] split ?prec _a = raise Too_small

let[@warning "-27"] split_along ?prec _a _ = raise Too_small

let[@warning "-27"] split_diff ?prec _a = raise Too_small

let volume _x = 0.

let diff = None

let render _ = failwith "Alias.render can not render"

let eval _ _ = raise Top_found
