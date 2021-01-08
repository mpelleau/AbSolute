open Tools
open Csp

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
  | GT -> Format.fprintf fmt ">"
  | LT -> Format.fprintf fmt "<"

let print_typ fmt = function
  | Int -> Format.fprintf fmt "int"
  | Real -> Format.fprintf fmt "real"

let print_var = Format.pp_print_string

let print_dom fmt = function
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

let print_assign fmt assignations =
  Format.fprintf fmt "Variables:\n" ;
  List.iter
    (fun (a, b, c) ->
      Format.fprintf fmt "%a %a in %a\n" print_typ a print_var b print_dom c)
    assignations

let print_cst fmt (a, b) =
  match (a, b) with
  | a, b when a = b -> Format.fprintf fmt "%a" Q.pp_print a
  | a, b -> Format.fprintf fmt "[%a; %a]" Q.pp_print a Q.pp_print b

let print_csts fmt (a, b) = Format.fprintf fmt "%a = %a" print_var a print_cst b

let rec print_all_csts fmt = function
  | [] -> ()
  | [a] -> Format.fprintf fmt "%a" print_csts a
  | a :: tl ->
      Format.fprintf fmt "%a " print_csts a ;
      print_all_csts fmt tl

let rec print_expr fmt = function
  | Funcall (name, args) ->
      let print_args fmt =
        Format.pp_print_list
          ~pp_sep:(fun fmt () -> Format.fprintf fmt ",")
          print_expr fmt
      in
      Format.fprintf fmt "%s(%a)" name print_args args
  | Neg e -> Format.fprintf fmt "(- %a)" print_expr e
  | Binary (b, e1, e2) ->
      Format.fprintf fmt "(%a %a %a)" print_expr e1 print_binop b print_expr e2
  | Var v -> Format.fprintf fmt "%s" v
  | Cst c -> Format.fprintf fmt "%a" Q.pp_print c

let print_cmp fmt (e1, c, e2) =
  Format.fprintf fmt "%a %a %a" print_expr e1 print_cmpop c print_expr e2

let rec print_bexpr fmt = function
  | Cmp c -> print_cmp fmt c
  | And (b1, b2) -> Format.fprintf fmt "%a && %a" print_bexpr b1 print_bexpr b2
  | Or (b1, b2) -> Format.fprintf fmt "%a || %a" print_bexpr b1 print_bexpr b2
  | Not b -> Format.fprintf fmt "not %a" print_bexpr b

let print_constraints fmt constraints =
  Format.fprintf fmt "Constraints:\n" ;
  List.iter (fun c -> Format.fprintf fmt "%a\n" print_bexpr c) constraints

let print_jacob fmt (v, e) =
  Format.fprintf fmt "\t(%a, %a)" print_var v print_expr e

let print fmt prog =
  Format.fprintf fmt "%a\n" print_assign prog.init ;
  Format.fprintf fmt "%a\n" print_constraints prog.constraints

let instance fmt (instance : Csp.instance) =
  let bind fmt (var, value) =
    Format.fprintf fmt "%s : %a" var Q.pp_print value
  in
  VarMap.bindings instance
  |> Format.fprintf fmt "{%a}" (Format.pp_print_list ~pp_sep:newline_sep bind)
