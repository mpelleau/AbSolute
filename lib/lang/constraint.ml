(** arithmetic comparison operators *)
type cmpop = EQ | LEQ | GEQ | NEQ | GT | LT

(** boolean comparison : e1 <> e2 *)
type comparison = Expr.t * cmpop * Expr.t

(** boolean expressions *)
type 'a boolean =
  | Cmp of 'a
  | And of 'a boolean * 'a boolean
  | Or of 'a boolean * 'a boolean
  | Not of 'a boolean

(** type for constraints *)
type t = comparison boolean

(** {1 Constructors} *)
let leq e1 e2 = Cmp (e1, LEQ, e2)

let lt e1 e2 = Cmp (e1, LT, e2)

let geq e1 e2 = Cmp (e1, GEQ, e2)

let gt e1 e2 = Cmp (e1, GT, e2)

let eq e1 e2 = Cmp (e1, EQ, e2)

let neq e1 e2 = Cmp (e1, NEQ, e2)

(** constraint for variable assignment by a constant *)
let assign var value = eq (Expr.var var) (Expr.of_mpqf value)

(** constraint for 'e \in [low;high]' *)
let inside v low high = And (geq v low, leq v high)

(** constraint for 'not (v \in [low;high])' *)
let outside v low high = Or (lt v low, gt v high)

(** same as inside but with constants instead of expressions' *)
let inside_cst v low high = Expr.(inside (var v) (of_mpqf low) (of_mpqf high))

(** constraint for 'not (v \in [low;high])' *)
let outside_cst v low high = Expr.(outside (var v) (of_mpqf low) (of_mpqf high))

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

(** {1 Printing} *)

(** comparison operator printer *)
let pp_cmpop fmt = function
  | EQ -> Format.fprintf fmt "="
  | LEQ -> Format.fprintf fmt "<="
  | GEQ -> Format.fprintf fmt ">="
  | NEQ -> Format.fprintf fmt "<>"
  | GT -> Format.fprintf fmt ">"
  | LT -> Format.fprintf fmt "<"

let pp_comparison fmt ((e1, c, e2) : comparison) =
  Format.fprintf fmt "%a %a %a" Expr.print e1 pp_cmpop c Expr.print e2

let rec print fmt : t -> unit = function
  | Cmp c -> pp_comparison fmt c
  | And (b1, b2) -> Format.fprintf fmt "%a && %a" print b1 print b2
  | Or (b1, b2) -> Format.fprintf fmt "%a || %a" print b1 print b2
  | Not b -> Format.fprintf fmt "not %a" print b
