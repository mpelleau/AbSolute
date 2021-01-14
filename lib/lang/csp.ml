open Tools

(** This module defines the main types used for the constraint language of
    AbSolute, along with the type of problems, and instance.*)

(** {1 Types} *)

(** variables are identified by a string *)
type var = string

(** constants are rationals (the domain of the variable *)
type i = Mpqf.t

(** Types of variables *)
type typ = Int | Real

(** binary arithmetic operators *)
type binop = ADD | SUB | MUL | DIV | POW

(** arithmetic comparison operators *)
type cmpop = EQ | LEQ | GEQ | NEQ | GT | LT

(** numeric expressions (function call, unary negation, binary operations,
    variables and constants)*)
type expr =
  | Funcall of var * expr list
  | Neg of expr
  | Binary of binop * expr * expr
  | Var of var
  | Cst of i

(** boolean comparison : e1 <> e2 *)
type comparison = expr * cmpop * expr

(** boolean expressions *)
type 'a boolean =
  | Cmp of 'a
  | And of 'a boolean * 'a boolean
  | Or of 'a boolean * 'a boolean
  | Not of 'a boolean

(** type for constraints *)
type bexpr = comparison boolean

(** domains of variables *)
type dom =
  | Finite of i * i  (** \[a;b\] *)
  | Minf of i  (** \]-oo; a\] *)
  | Inf of i  (** \[a; +oo\[ *)
  | Set of i list  (** \{x1; x2; ...; xn\} *)
  | Top  (** \]-oo; +oo\[ *)

(** declaration *)
type decl = typ * var * dom

(** the instance type *)
type instance = i VarMap.t

(** annotations to test the validity of the solver *)
type info =
  | Exact of instance list
  | Unfeasible
  | Known of (instance * bool) list

(** type of constraint satisfaction problems *)
type problem =
  { init: decl list
  ; constraints: bexpr list
  ; objective: expr option
  ; solutions: info option (* extra information about feasbility *) }

(** {1 Accessors} *)

(** computes the list of all variable names *)
let get_var_names p = List.map (fun (_, v, _) -> v) p.init

(** {1 Constructors}*)

(** empty problem, with no variables and no constraints *)
let empty = {init= []; constraints= []; objective= None; solutions= None}

(** initalizes and unconstrained CSP *)
let initialize (variables : (typ * var * dom) list) : problem =
  {init= variables; constraints= []; objective= None; solutions= None}

(** adds a real variable in the csp *)
let add_real_var name inf sup csp =
  let assign = (Real, name, Finite (inf, sup)) in
  {csp with init= assign :: csp.init}

(** adds a real variable in the csp, with float bounds *)
let add_real_var_f s l h = add_real_var s (Mpqf.of_float l) (Mpqf.of_float h)

(** adds an integer variable in the csp *)
let add_int_var name inf sup csp =
  let assign = (Int, name, Finite (inf, sup)) in
  {csp with init= assign :: csp.init}

(** adds an integer variable in the csp with integer bounds *)
let add_int_var_i s l h = add_int_var s (Mpqf.of_int l) (Mpqf.of_int h)

(** adds a constraint to the csp *)
let add_constr c csp = {csp with constraints= c :: csp.constraints}
