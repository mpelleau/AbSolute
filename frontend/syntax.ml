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

let print_unop fmt = function
  | NEG -> Format.fprintf fmt "-"
  | SQRT -> Format.fprintf fmt "sqrt"
  | COS -> Format.fprintf fmt "cos"
  | SIN -> Format.fprintf fmt "sin"

let print_typ fmt = function
  | INT ->  Format.fprintf fmt "int"
  | REAL ->  Format.fprintf fmt "real"

let print_var fmt s = Format.fprintf fmt "%s" s

let print_dom fmt (a,b) = Format.fprintf fmt "[%f; %f]" a b

let print_assign fmt (a,b,c) = 
  Format.fprintf fmt "%a %a=%a;\n" print_typ a print_var b print_dom c

let print fmt prog = 
  match prog.init with
  | [] -> ()
  | a::tl -> Format.fprintf fmt "%a%!" print_assign a
