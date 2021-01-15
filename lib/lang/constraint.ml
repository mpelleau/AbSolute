(** binary arithmetic operators *)
type binop = ADD | SUB | MUL | DIV | POW

(** arithmetic comparison operators *)
type cmpop = EQ | LEQ | GEQ | NEQ | GT | LT

(** numeric expressions (function call, unary negation, binary operations,
    variables and constants)*)
type expr =
  | Funcall of string * expr list
  | Neg of expr
  | Binary of binop * expr * expr
  | Var of string
  | Cst of Q.t

(** boolean comparison : e1 <> e2 *)
type comparison = expr * cmpop * expr

(** boolean expressions *)
type 'a boolean =
  | Cmp of 'a
  | And of 'a boolean * 'a boolean
  | Or of 'a boolean * 'a boolean
  | Not of 'a boolean

(** type for constraints *)
type t = comparison boolean

(** {1 Constants}*)
let one = Cst Q.one

let zero = Cst Q.zero

let two = Cst Q.two

(** {1 Expression Constructors} *)

(** builds an expression from an integer *)
let of_int n = Cst (Q.of_int n)

(** builds an expression from an float *)
let of_float f = Cst (Q.of_float f)

(** builds an expression from an Mpqf.t *)
let of_mpqf m = Cst m

(** given an expression [e] builds the expresspion for [e*e]*)
let square expr = Binary (POW, expr, two)

(** {1 Constructors} *)

(** {2 comparisons} *)
let leq e1 e2 = Cmp (e1, LEQ, e2)

let lt e1 e2 = Cmp (e1, LT, e2)

let geq e1 e2 = Cmp (e1, GEQ, e2)

let gt e1 e2 = Cmp (e1, GT, e2)

let eq e1 e2 = Cmp (e1, EQ, e2)

let neq e1 e2 = Cmp (e1, NEQ, e2)

(** constraint for variable assignment by a constant *)
let assign var value = eq (Var var) (of_mpqf value)

(** constraint for 'e \in [low;high]' *)
let inside v low high = And (geq v low, leq v high)

(** constraint for 'not (v \in [low;high])' *)
let outside v low high = Or (lt v low, gt v high)

(** same as inside but with constants instead of expressions' *)
let inside_cst v low high = inside (Var v) (of_mpqf low) (of_mpqf high)

(** constraint for 'not (v \in [low;high])' *)
let outside_cst v low high = outside (Var v) (of_mpqf low) (of_mpqf high)

(** {1 Operations} *)

(** cmp operator inversion *)
let inv = function
  | EQ -> EQ
  | LEQ -> GEQ
  | GEQ -> LEQ
  | NEQ -> NEQ
  | GT -> LT
  | LT -> GT

(** comparison operator negation *)
let neg = function
  | EQ -> NEQ
  | LEQ -> GT
  | GEQ -> LT
  | NEQ -> EQ
  | GT -> LEQ
  | LT -> GEQ
