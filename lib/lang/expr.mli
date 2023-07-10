(** This module defines the numerical language and some basic operations over it*)

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

(** generic type for annotated expressions *)
type 'a annot =
  | AFuncall of string * 'a annot_t list
  | ANeg of 'a annot_t
  | ABinary of binop * 'a annot_t * 'a annot_t
  | AVar of string
  | ACst of Q.t

and 'a annot_t = 'a annot * 'a

(** {1 Errors} *)

(** raised by evaluation functions *)
exception Division_by_zero

(** raised by evaluation functions *)
exception Non_integer_exposant

(** {1 Constructors} *)

(** {2 Constants}*)

val one : t

val zero : t

val two : t

(** {2 Expression Constructors} *)

val of_int : int -> t
(** builds an expression from an integer *)

val of_float : float -> t
(** builds an expression from an float *)

val of_mpqf : Q.t -> t
(** builds an expression from an Mpqf.t *)

val var : string -> t
(** variables constructor *)

val add : t -> t -> t
(** addition *)

val sub : t -> t -> t
(** substraction *)

val mul : t -> t -> t
(** multiplication *)

val div : t -> t -> t
(** multiplication *)

val pow : t -> t -> t
(** given an expression [e] builds the expresspion for [e1^e2]*)

val square : t -> t
(** given an expression [e] builds the expresspion for [e*e]*)

(** {1 Predicates}*)

val has_variable : t -> bool
(** checks if an expression contains a variable *)

val is_linear : t -> bool
(** checks if an expression is linear:

    - is_linear (2*x + 4*y) = true
    - is_linear (2*x*z + y) = false *)

(** {1 Operations} *)

val op_to_fun : binop -> Q.t -> Q.t -> Q.t
(** convert a binary operator to a rational function. The resulting function may
    raise Division_by_zero or Non_integer_exposant when the binary operator is
    respectivelly DIV or POW *)

val constant_propagation : t -> t
(** bottom-up partial evaluation of expressions when operations involving only
    constants are involved:

    - constant_propagation (x+2) = (x+2)
    - constant_propagation (2+2)*x = (4*x) *)

val collect_vars : t -> int Tools.VarMap.t
(** Returns all the variables appearing in an expression as a map where to each
    variable is associated the integer number of its occurences *)

val replace : ?simplify:bool -> t -> string -> t -> t
(** [replace expr var value] builds a new expression identical to [expr] where
    all the occurences of the variable [var] are replaced by the expression
    [value].

    if simplify is true (default behaviour), a round of constant propagation is
    applied on the resulting expression *)

val fix_var : ?simplify:bool -> t -> string -> Q.t -> t
(** [fix_var expr var cst] builds a new expression identical to [expr] where all
    the occurences of the variable [var] are replaced by the constant [cst]

    if simplify is true (default behaviour), a round of constant propagation is
    applied on the resulting expression *)

val eval : t -> Instance.t -> Q.t
(** Evaluates the expression at the given point.

    @raise [Invalid_arg]
      if a division by zero occurs of if an exponentitation by a non integer
      exposant is made. *)

val deannot : 'a annot_t -> t
(** removes annotations from an annotated expression *)

(** {1 Printing} *)

val pp_var : Format.formatter -> string -> unit
(** variables printing *)

val pp_binop : Format.formatter -> binop -> unit
(** binary operators printing *)

val print : Format.formatter -> t -> unit
(** expression printer *)

val to_string : t -> string
(** Conversion to a string *)
