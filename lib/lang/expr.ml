(** This module defines the numerical language, and some basic operations over
    it*)

(** binary arithmetic operators *)
type binop = ADD | SUB | MUL | DIV | POW

(** numeric expressions (function call, unary negation, binary operations,
    variables and constants)*)
type t =
  | Funcall of string * t list
  | Neg of t
  | Binary of binop * t * t
  | Var of string
  | Cst of Q.t

(** {1 Constructors} *)

(** {2 Constants}*)
let one = Cst Q.one

let zero = Cst Q.zero

let two = Cst Q.two

(** {2 Expression Constructors} *)

(** builds an expression from an integer *)
let of_int n = Cst (Q.of_int n)

(** builds an expression from an float *)
let of_float f = Cst (Q.of_float f)

(** builds an expression from an Mpqf.t *)
let of_mpqf m = Cst m

(** given an expression [e] builds the expresspion for [e*e]*)
let square expr = Binary (POW, expr, two)

(** variables constructor *)
let var v = Var v

(** {1 Printing} *)

(** variables printing *)
let pp_var = Format.pp_print_string

(** binary operators printing *)
let pp_binop fmt = function
  | ADD -> Format.fprintf fmt "+"
  | SUB -> Format.fprintf fmt "-"
  | MUL -> Format.fprintf fmt "*"
  | DIV -> Format.fprintf fmt "/"
  | POW -> Format.fprintf fmt "^"

(** expression printer *)
let rec print fmt = function
  | Funcall (name, args) ->
      let print_args fmt =
        Format.pp_print_list
          ~pp_sep:(fun fmt () -> Format.fprintf fmt ",")
          print fmt
      in
      Format.fprintf fmt "%s(%a)" name print_args args
  | Neg e -> Format.fprintf fmt "(- %a)" print e
  | Binary (b, e1, e2) ->
      Format.fprintf fmt "(%a %a %a)" print e1 pp_binop b print e2
  | Var v -> Format.fprintf fmt "%s" v
  | Cst c -> Format.fprintf fmt "%a" Q.pp_print c

(** {1 Predicates}*)

(** checks if an expression contains a variable *)
let rec has_variable = function
  | Funcall (_, args) -> List.exists has_variable args
  | Neg e -> has_variable e
  | Binary (_, e1, e2) -> has_variable e1 || has_variable e2
  | Var _ -> true
  | Cst _ -> false

(** checks if an expression is linear *)
let rec is_linear = function
  | Neg e -> is_linear e
  | Binary (MUL, e1, e2) | Binary (DIV, e1, e2) ->
      (not (has_variable e1 && has_variable e2)) && is_linear e1 && is_linear e2
  | Binary (POW, e1, e2) -> not (has_variable e1 || has_variable e2)
  | Binary (_, e1, e2) -> is_linear e1 && is_linear e2
  | Var _ | Cst _ -> true
  | _ -> false
