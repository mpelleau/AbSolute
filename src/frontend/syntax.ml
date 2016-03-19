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
  | Cst of i

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


(*************************************************************)
(*                    PRINTING UTILITIES                     *)
(*************************************************************)

let print_unop fmt = function
  | NEG -> Format.fprintf fmt "-"
  | SQRT -> Format.fprintf fmt "sqrt"
  | COS -> Format.fprintf fmt "cos"
  | SIN -> Format.fprintf fmt "sin"

let print_binop fmt = function
  | ADD -> Format.fprintf fmt "+"
  | SUB -> Format.fprintf fmt "-"
  | MUL -> Format.fprintf fmt "*"
  | DIV -> Format.fprintf fmt "/"
  | POW -> Format.fprintf fmt "^"

let print_cmpop fmt = function
  | EQ -> Format.fprintf fmt "=" 
  | LEQ -> Format.fprintf fmt "<="
  | GEQ -> Format.fprintf fmt ">="
  | NEQ -> Format.fprintf fmt "<>"
  | GT ->  Format.fprintf fmt ">"
  | LT -> Format.fprintf fmt "<"

let print_typ fmt = function
  | INT ->  Format.fprintf fmt "int"
  | REAL ->  Format.fprintf fmt "real"

let print_var fmt s = Format.fprintf fmt "%s" s

let print_dom fmt (a,b) = Format.fprintf fmt "[%.2f; %.2f]" a b

let print_assign fmt (a,b,c) = 
  Format.fprintf fmt "%a %a=%a" print_typ a print_var b print_dom c

let rec print_expr fmt = function  
  | Unary (u, e) -> 
    Format.fprintf fmt "%a %a" print_unop u print_expr e
  | Binary (b, e1 , e2) -> 
    Format.fprintf fmt "%a %a %a" print_expr e1 print_binop b print_expr e2
  | Var v -> Format.fprintf fmt "%s" v
  | Cst c -> Format.fprintf fmt "%.2f" c

let rec print_bexpr fmt = function
  | Cmp (c,e1,e2) -> 
    Format.fprintf fmt "%a %a %a" print_expr e1 print_cmpop c print_expr e2
  | And (b1,b2) ->
    Format.fprintf fmt "%a && %a" print_bexpr b1 print_bexpr b2
  | Or  (b1,b2) ->
    Format.fprintf fmt "%a || %a" print_bexpr b1 print_bexpr b2
  | Not b -> Format.fprintf fmt "not %a" print_bexpr b

let print fmt prog = 
  let rec aux f = function
  | [] -> ()
  | a::tl -> Format.fprintf fmt "%a;\n" f a; aux f tl
  in 
  aux print_assign prog.init;
  Format.fprintf fmt "\n";
  aux print_bexpr prog.constraints
