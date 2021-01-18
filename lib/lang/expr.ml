(** This module defines the numerical language, and some basic operations over
    it*)

open Tools

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

(** {1 Operations} *)

(** Returns all the variables appearing in an expression as a map where to each
    variable is associated the (integer) number of its occurences *)
let rec collect_vars =
  let merge = VarMap.union (fun _v i1 i2 -> Some (i1 + i2)) in
  function
  | Neg e -> collect_vars e
  | Binary (_, e1, e2) -> merge (collect_vars e1) (collect_vars e2)
  | Cst _ -> VarMap.empty
  | Funcall (_, a) ->
      List.map collect_vars a |> List.fold_left merge VarMap.empty
  | Var v -> VarMap.singleton v 1

(** [fix_var expr var cst] builds a new expression identical to the [expr] where
    all the occurences of the variable [var] are replaced by the constant [cst] *)
let fix_var (e : t) v (c : Q.t) : t =
  let rec aux = function
    | Funcall (name, args) -> Funcall (name, List.map aux args)
    | Neg e -> aux e
    | Binary (b, e1, e2) -> Binary (b, aux e1, aux e2)
    | Var v' as var -> if v' = v then Cst c else var
    | cst -> cst
  in
  aux e

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
