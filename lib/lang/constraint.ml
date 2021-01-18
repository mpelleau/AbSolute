(** This module defines the constraint language, and some basic operations over
    it*)

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
let leq e1 e2 : t = Cmp (e1, LEQ, e2)

let lt e1 e2 : t = Cmp (e1, LT, e2)

let geq e1 e2 : t = Cmp (e1, GEQ, e2)

let gt e1 e2 : t = Cmp (e1, GT, e2)

let eq e1 e2 : t = Cmp (e1, EQ, e2)

let neq e1 e2 : t = Cmp (e1, NEQ, e2)

(** constraint for variable assignment by a constant *)
let assign var value : t = eq (Expr.var var) (Expr.of_mpqf value)

(** constraint for 'e \in [low;high]' *)
let inside v low high : t = And (geq v low, leq v high)

(** constraint for 'not (v \in [low;high])' *)
let outside v low high : t = Or (lt v low, gt v high)

(** same as inside but with constants instead of expressions' *)
let inside_cst v l h : t = Expr.(inside (var v) (of_mpqf l) (of_mpqf h))

(** constraint for 'not (v \in [low;high])' *)
let outside_cst v l h : t = Expr.(outside (var v) (of_mpqf l) (of_mpqf h))

(** {1 Operations} *)

(** cmp operator inversion *)
let inv_cmp = function
  | EQ -> EQ
  | LEQ -> GEQ
  | GEQ -> LEQ
  | NEQ -> NEQ
  | GT -> LT
  | LT -> GT

(** comparison operator negation *)
let neg_cmp = function
  | EQ -> NEQ
  | LEQ -> GT
  | GEQ -> LT
  | NEQ -> EQ
  | GT -> LEQ
  | LT -> GEQ

(** constraint negation *)
let rec neg : t -> t = function
  | Cmp (e1, op, e2) -> Cmp (e1, neg_cmp op, e2)
  | And (b1, b2) -> Or (neg b1, neg b2)
  | Or (b1, b2) -> And (neg b1, neg b2)
  | Not b -> b

(** rewrites a constraint into an equivalent constraint without 'Not' *)
let rec remove_not : t -> t = function
  | Not b -> remove_not (neg b)
  | And (b1, b2) -> And (remove_not b1, remove_not b2)
  | Or (b1, b2) -> Or (remove_not b1, remove_not b2)
  | x -> x

(** Returns all the variables appearing in a constraint as a map where to each
    variable is associated the (integer) number of occurences *)
let rec collect_vars =
  let merge = Tools.VarMap.union (fun _v i1 i2 -> Some (i1 + i2)) in
  function
  | Not b -> collect_vars (neg b)
  | And (b1, b2) | Or (b1, b2) -> merge (collect_vars b1) (collect_vars b2)
  | Cmp (e1, _, e2) -> merge (Expr.collect_vars e1) (Expr.collect_vars e2)

(** [fix_var constr var cst] builds a new constraint identical to [constr] where
    all the occurences of the variable [var] are replaced by the constant [cst] *)
let fix_var (constr : t) v c : t =
  let rec aux = function
    | Cmp (e1, cmp, e2) -> Cmp (Expr.fix_var e1 v c, cmp, Expr.fix_var e2 v c)
    | And (c1, c2) -> And (aux c1, aux c2)
    | Or (c1, c2) -> Or (aux c1, aux c2)
    | Not c -> Not (aux c)
  in
  aux constr

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
