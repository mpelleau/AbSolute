(* variables are identified by a string *)
type var = string

(* constants are intervals (the domain of the variable *)
type i = float

(* unary arithmetic operators *)
type unop = NEG | SQRT | COS | SIN

(* binary arithmetic operators *)
type binop = ADD | SUB | MUL | DIV | POW

(* arithmetic comparison operators *)
type cmpop =
  | EQ | LEQ | GEQ | NEQ | GT | LT

(* numeric expressions *)
type expr =
  | Unary of unop * expr
  | Binary of binop * expr * expr
  | Var of var
  | Cst of i * i

(* boolean expressions *)
type bexpr =
  | Cmp of cmpop * expr * expr
  | And of bexpr * bexpr
  | Or of bexpr * bexpr
  | Not of bexpr

type typ = INT | REAL

type dom = i*i 

(* assign *)
type assign = (typ * var * dom)

(* declarations *)
type decls =  assign list

(* statements *)
type constrs = bexpr list

(* program *)
type prog = { init: decls; constraints: constrs}
