open Tools

(* variables are identified by a string *)
type var = string

(* constants are rationals (the domain of the variable *)
type i = Mpqf.t

type annot = Int | Real

(* unary arithmetic operators *)
type unop = NEG

(* binary arithmetic operators *)
type binop = ADD | SUB | MUL | DIV | POW

(* arithmetic comparison operators *)
type cmpop =
  | EQ | LEQ | GEQ | NEQ | GT | LT

(* numeric expressions *)
type expr =
  | Funcall of var * expr list
  | Unary   of unop * expr
  | Binary  of binop * expr * expr
  | Var     of var
  | Cst     of i

(* boolean expressions *)
type bexpr =
  | Cmp of cmpop * expr * expr
  | And of bexpr * bexpr
  | Or  of bexpr * bexpr
  | Not of bexpr

type dom = Finite of i * i   (* [a;b] *)
         | Minf   of i       (* [-oo; a] *)
         | Inf    of i       (* [a; +oo] *)
         | Set    of i list  (* {x1; x2; ...; xn} *)
         | Top               (* [-oo; +oo] *)

(* assign *)
type assign = (annot * var * dom)

(* declarations *)
type decls =  assign list

(* statements *)
type constrs = bexpr list

(* jacobian *)
type jacob = (var * expr) list

type ctrs = (bexpr * jacob) list

(* constants *)
type csts = (var * (i*i)) list

(* the instance type *)
type instance = i VarMap.t

(* we can annotate a problem with information on the resolution,
   to check the soundness of the solver *)
(* A solution_info is either Some (l), where l is instance list,
   of known solution and known no goods *)
(* or None, when the problem is infeasible *)
type solution_info =
  (instance * bool) list option

(* program *)
type prog = {
    init        : decls;
    constants   : csts;
    objective   : expr;
    constraints : constrs;
    jacobian    : ctrs;
    to_draw     : var list;
    view        : jacob;
    solutions   : solution_info (* extra information about the solutions of te problem *)
  }
