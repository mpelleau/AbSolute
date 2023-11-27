(** This module defines the main types used for the constraint language of
    AbSolute, along with the type of problems, and instance.*)

open Tools

(** {1 Types} *)

(** Types of variables *)
type typ = Int | Real

(** declaration *)
type decl = typ * string * Dom.t

(** the instance type *)
type instance = Q.t VarMap.t

(** annotations to test the validity of the solver *)
type info =
  | Exact of Instance.t list
  | Unfeasible
  | Known of (Instance.t * bool) list

(** type of constraint satisfaction problems *)
type t =
  { variables: decl list
  ; constraints: Constraint.t list
  ; objective: Expr.t option
  ; solutions: info option (* extra information about feasbility *) }

(** {1 Accessors} *)

(** returns list of all variable names *)
let get_var_names p = List.map (fun (_, v, _) -> v) p.variables

(** {1 Constructors}*)

(** empty problem, with no variables and no constraints *)
let empty = {variables= []; constraints= []; objective= None; solutions= None}

(** initalizes and unconstrained CSP *)
let initialize (variables : (typ * string * Dom.t) list) : t =
  {variables; constraints= []; objective= None; solutions= None}

(** adds a real variable in the csp *)
let add_real_var name inf sup csp =
  let assign = (Real, name, Dom.interval inf sup) in
  {csp with variables= assign :: csp.variables}

(** adds a real variable in the csp, with float bounds *)
let add_real_var_f s l h = add_real_var s (Q.of_float l) (Q.of_float h)

(** adds an integer variable in the csp *)
let add_int_var name inf sup csp =
  let assign = (Int, name, Dom.interval inf sup) in
  {csp with variables= assign :: csp.variables}

(** adds an integer variable in the csp with integer bounds *)
let add_int_var_i s l h = add_int_var s (Q.of_int l) (Q.of_int h)

(** adds a constraint to the csp *)
let add_constr csp c = {csp with constraints= c :: csp.constraints}

(** {1 Operations} *)

(** Evaluates the CSP at the given point, i.e true iff all constraints are
    satisfied

    @raise [Invalid_arg]
      if a division by zero occurs or if an exponentitation by a non integer
      exposant is made. *)
let sat csp instance =
  List.for_all (fun c -> Constraint.eval c instance) csp.constraints

(** [fix_var csp var cst] builds a new csp identical to the [csp] where all the
    occurences of the variable [var] are replaced by the constant [cst], and
    where the variables [v] is removed from the csp variables.*)
let fix_var csp v cst =
  { csp with
    constraints=
      List.map (fun cstr -> Constraint.fix_var cstr v cst) csp.constraints
  ; objective= Option.map (fun expr -> Expr.fix_var expr v cst) csp.objective
  ; variables= List.filter (fun (_, v', _) -> v' <> v) csp.variables }

(** {1 Printing} *)

let pp_typ fmt = function
  | Int -> Format.fprintf fmt "int"
  | Real -> Format.fprintf fmt "real"

let pp_decl fmt (a, b, c) =
  Format.fprintf fmt "%a %s in %a" pp_typ a b Dom.print c

let pp_declarations = pp_list_sep "\n" pp_decl

let pp_constraints = pp_list_sep "\n" Constraint.print

let print fmt p =
  Format.fprintf fmt "Variables:%a\nConstraints:%a\n" pp_declarations
    p.variables pp_constraints p.constraints

let to_graphviz p output =
  let oc = open_out output in
  let fmt_oc = Format.formatter_of_out_channel oc in
  let print_edges fmt c =
    let label = Constraint.to_string c in
    let vars = Constraint.collect_vars c in
    VarMap.iter
      (fun v1 _ ->
        VarMap.iter
          (fun v2 _ ->
            if v1 <> v2 then
              Format.fprintf fmt "%s -- %s [label=\"%s\"];@," v1 v2 label )
          vars )
      vars
  in
  Format.fprintf fmt_oc "@[<v 2>graph G{@,%a@]@,}"
    (Format.pp_print_list print_edges)
    p.constraints
