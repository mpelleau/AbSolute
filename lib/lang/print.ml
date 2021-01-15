(** Printing functions for the different types in Csp *)

open Tools
open Csp
open Constraint
open Expr

let binop fmt = function
  | ADD -> Format.fprintf fmt "+"
  | SUB -> Format.fprintf fmt "-"
  | MUL -> Format.fprintf fmt "*"
  | DIV -> Format.fprintf fmt "/"
  | POW -> Format.fprintf fmt "^"

let cmpop fmt = function
  | EQ -> Format.fprintf fmt "="
  | LEQ -> Format.fprintf fmt "<="
  | GEQ -> Format.fprintf fmt ">="
  | NEQ -> Format.fprintf fmt "<>"
  | GT -> Format.fprintf fmt ">"
  | LT -> Format.fprintf fmt "<"

let typ fmt = function
  | Int -> Format.fprintf fmt "int"
  | Real -> Format.fprintf fmt "real"

let var = Format.pp_print_string

let dom fmt = function
  | Finite (a, b) -> Format.fprintf fmt "[%a; %a]" Q.print a Q.print b
  | Minf i -> Format.fprintf fmt "[-oo; %a]" Q.print i
  | Inf i -> Format.fprintf fmt "[%a; +oo]" Q.print i
  | Set l ->
      let print_set =
        Format.pp_print_list
          ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
          Q.print
      in
      Format.fprintf fmt "{%a}" print_set l
  | Top -> Format.fprintf fmt "[-oo; +oo]"

let declarations fmt (d : decl list) =
  List.iter
    (fun (a, b, c) -> Format.fprintf fmt "%a %a in %a\n" typ a var b dom c)
    d

let rec expr fmt = function
  | Funcall (name, args) ->
      let print_args fmt =
        Format.pp_print_list
          ~pp_sep:(fun fmt () -> Format.fprintf fmt ",")
          expr fmt
      in
      Format.fprintf fmt "%s(%a)" name print_args args
  | Neg e -> Format.fprintf fmt "(- %a)" expr e
  | Binary (b, e1, e2) ->
      Format.fprintf fmt "(%a %a %a)" expr e1 binop b expr e2
  | Var v -> Format.fprintf fmt "%s" v
  | Cst c -> Format.fprintf fmt "%a" Q.pp_print c

let comparison fmt ((e1, c, e2) : comparison) =
  Format.fprintf fmt "%a %a %a" expr e1 cmpop c expr e2

let rec bexpr fmt : Constraint.t -> unit = function
  | Cmp c -> comparison fmt c
  | And (b1, b2) -> Format.fprintf fmt "%a && %a" bexpr b1 bexpr b2
  | Or (b1, b2) -> Format.fprintf fmt "%a || %a" bexpr b1 bexpr b2
  | Not b -> Format.fprintf fmt "not %a" bexpr b

let constraints fmt = List.iter (Format.fprintf fmt "%a\n" bexpr)

let prob fmt p =
  Format.fprintf fmt "Variables:%a\nConstraints:%a\n" declarations p.init
    constraints p.constraints

let instance fmt (instance : Csp.instance) =
  let bind fmt (var, value) =
    Format.fprintf fmt "%s : %a" var Q.pp_print value
  in
  VarMap.bindings instance
  |> Format.fprintf fmt "{%a}" (Format.pp_print_list ~pp_sep:newline_sep bind)
