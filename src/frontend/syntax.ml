(* variables are identified by a string *)
type var = string

(* constants are intervals (the domain of the variable *)
type i = float

(* unary arithmetic operators *)
type unop = NEG | SQRT | ABS | COS | SIN

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

type dom = Finite of i*i | Minf of i | Inf of i | Top

(* assign *)
type assign = (typ * var * dom)

(* declarations *)
type decls =  assign list

(* statements *)
type constrs = bexpr list

(* program *)
type prog = { init: decls; constraints: constrs; to_draw : (var * var) option}


(*****************************************)
(*        USEFUL FUNCTION ON AST         *)
(*****************************************)

(* power unrolling on exprs*)
let rec power_unrolling expr : expr =
  let rec doit res e1 i = 
    match i with
    | 0 -> Cst 1.
    | 1 -> res
    | _ -> doit (Binary(MUL,res,res)) e1 (i-1)
  in
  match expr with
  | Binary (POW,e1,Cst x) when ceil x = x && x >= 0. -> doit e1 e1 (int_of_float x)
  | Unary (u, e) -> Unary (u,(power_unrolling e))
  | Binary (b,e1,e2) -> Binary(b,(power_unrolling e1), (power_unrolling e2))
  | x -> x

(* power unrolling on bexprs*)
let rec power_unrolling_bexpr bexpr : bexpr =
  match bexpr with
  | Cmp (c,e1,e2) -> Cmp(c, (power_unrolling e1), (power_unrolling e2))
  | And (b1,b2) -> And (power_unrolling_bexpr b1, power_unrolling_bexpr b2)
  | Or  (b1,b2) -> Or (power_unrolling_bexpr b1, power_unrolling_bexpr b2)
  | Not b -> Not (power_unrolling_bexpr b)

(* cmp operator negation *)
let neg = function
| EQ -> NEQ
| LEQ ->GT
| GEQ ->LT
| NEQ ->EQ
| GT -> LEQ
| LT -> GEQ

(* constraint negation *)
let rec neg_bexpr = function
  | Cmp (op,e1,e2) -> Cmp(neg op,e1,e2)
  | And (b1,b2) -> Or (neg_bexpr b1, neg_bexpr b2)
  | Or (b1,b2) -> And (neg_bexpr b1, neg_bexpr b2)
  | Not b -> b

(* checks if a expression is linear *)
let rec is_linear = function  
  | Unary (NEG,e) -> is_linear e
  | Binary(b, e1, e2) when b <> POW -> is_linear e1 && is_linear e2
  | Var _ | Cst _ -> true
  | _ -> false

(* checks if a constraints is linear *)
let rec is_cons_linear = function
  | Cmp (_,e1,e2) -> is_linear e1 && is_linear e2
  | And (b1,b2) -> is_cons_linear b1 && is_cons_linear b2
  | Or (b1,b2) -> is_cons_linear b1 && is_cons_linear b2
  | Not b -> is_cons_linear b


(*************************************************************)
(*                    PRINTING UTILITIES                     *)
(*************************************************************)

let print_unop fmt = function
  | NEG -> Format.fprintf fmt "-"
  | SQRT -> Format.fprintf fmt "sqrt"
  | COS -> Format.fprintf fmt "cos"
  | SIN -> Format.fprintf fmt "sin"
  | ABS -> Format.fprintf fmt "abs"

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

let print_dom fmt = function
  | Finite (a,b) ->  Format.fprintf fmt "[%.2f; %.2f]" a b
  | Minf i -> Format.fprintf fmt "[-oo; %.2f]" i
  | Inf i -> Format.fprintf fmt "[%.2f; 00]" i
  | Top -> Format.fprintf fmt "[-oo; 00]"

let print_assign fmt (a,b,c) = 
  Format.fprintf fmt "%a %a=%a" print_typ a print_var b print_dom c

let rec print_expr fmt = function  
  | Unary (NEG, e) -> 
    Format.fprintf fmt "(- %a)" print_expr e
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
