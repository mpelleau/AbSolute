open Tools

(* variables are identified by a string *)
type var = string

(* constants are rationals (the domain of the variable *)
type i = Mpqf.t

type annot = Int | Real

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
type decl = annot * var * dom

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

(* program *)
type prog =
  { init: decls
  ; constraints: constrs
  ; objective: expr option
  ; solutions: info option (* extra information about feasbility *) }
