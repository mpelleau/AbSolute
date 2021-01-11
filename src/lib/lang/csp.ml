open Tools

(* variables are identified by a string *)
type var = string

(* constants are rationals (the domain of the variable *)
type i = Mpqf.t

type typ = Int | Real

(* binary arithmetic operators *)
type binop = ADD | SUB | MUL | DIV | POW

(* arithmetic comparison operators *)
type cmpop = EQ | LEQ | GEQ | NEQ | GT | LT

(* numeric expressions (function call, unary negation, binary operations,
   variables and constants)*)
type expr =
  | Funcall of var * expr list
  | Neg of expr
  | Binary of binop * expr * expr
  | Var of var
  | Cst of i

(* boolean expression : e1 <> e2 *)
type comparison = expr * cmpop * expr

(* boolean expressions *)
type 'a boolean =
  | Cmp of 'a
  | And of 'a boolean * 'a boolean
  | Or of 'a boolean * 'a boolean
  | Not of 'a boolean

type bexpr = comparison boolean

type dom =
  | Finite of i * i  (** [a;b] *)
  | Minf of i  (** [-oo; a] *)
  | Inf of i  (** [a; +oo] *)
  | Set of i list  (** [x1; x2; ...; xn] *)
  | Top  (** [-oo; +oo] *)

(* declaration *)
type decl = typ * var * dom

(* declarations *)
type decls = decl list

(* statements *)
type constrs = bexpr list

(* the instance type *)
type instance = i VarMap.t

(* annotations to test the validity of the solver *)
type info =
  | Exact of instance list
  | Unfeasible
  | Known of (instance * bool) list

(* problem *)
type problem =
  { init: decls
  ; constraints: constrs
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

(** adds a real bounded variable in the csp *)
let add_real_var csp name inf sup =
  let assign = (Real, name, Finite (inf, sup)) in
  {csp with init= assign :: csp.init}

(** adds an integer bounded variable in the csp *)
let add_int_var csp name inf sup =
  let assign = (Int, name, Finite (inf, sup)) in
  {csp with init= assign :: csp.init}

(** adds a constraint to the csp *)
let add_constr csp c = {csp with constraints= c :: csp.constraints}
