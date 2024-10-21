open Tools

type binop = ADD | SUB | MUL | DIV | POW

type t =
  | Funcall of string * t list
  | Neg of t
  | Binary of binop * t * t
  | Var of string
  | Cst of Q.t

type 'a annot =
  | AFuncall of string * 'a annot_t list
  | ANeg of 'a annot_t
  | ABinary of binop * 'a annot_t * 'a annot_t
  | AVar of string
  | ACst of Q.t

and 'a annot_t = 'a annot * 'a

exception Division_by_zero

exception Non_integer_exposant

exception Arity_error

let one = Cst Q.one

let zero = Cst Q.zero

let two = Cst Q.two

let of_int n = Cst (Q.of_int n)

let of_float f = Cst (Q.of_float f)

let of_mpqf m = Cst m

let var v = Var v

let add e1 e2 = Binary (ADD, e1, e2)

let sub e1 e2 = Binary (SUB, e1, e2)

let mul e1 e2 = Binary (MUL, e1, e2)

let div e1 e2 = Binary (DIV, e1, e2)

let pow e1 e2 = Binary (POW, e1, e2)

let square expr = Binary (POW, expr, two)

let funcall f args = Funcall (f, args)

let abs x = Funcall ("abs", [x])

let rec has_variable = function
  | Funcall (_, args) -> List.exists has_variable args
  | Neg e -> has_variable e
  | Binary (_, e1, e2) -> has_variable e1 || has_variable e2
  | Var _ -> true
  | Cst _ -> false

let rec is_linear = function
  | Neg e -> is_linear e
  | Binary (MUL, e1, e2) ->
      (not (has_variable e1 && has_variable e2)) && is_linear e1 && is_linear e2
  | Binary (DIV, e1, e2) ->
      (not (has_variable e2)) && is_linear e1 && is_linear e2
  | Binary (POW, e1, e2) -> not (has_variable e1 || has_variable e2)
  | Binary (_, e1, e2) -> is_linear e1 && is_linear e2
  | Var _ | Cst _ -> true
  | _ -> false

let rec collect_vars =
  let merge = VarMap.union (fun _v i1 i2 -> Some (i1 + i2)) in
  function
  | Neg e -> collect_vars e
  | Binary (_, e1, e2) -> merge (collect_vars e1) (collect_vars e2)
  | Cst _ -> VarMap.empty
  | Funcall (_, a) ->
      List.map collect_vars a |> List.fold_left merge VarMap.empty
  | Var v -> VarMap.singleton v 1

let op_to_fun = function
  | ADD -> Q.add
  | SUB -> Q.sub
  | MUL -> Q.mul
  | DIV -> (
      fun a b ->
        match Q.div a b with None -> raise Division_by_zero | Some q -> q )
  | POW -> (
      fun a b ->
        match Q.to_int b with
        | None -> raise Non_integer_exposant
        | Some e -> Q.pow a e )

let rec constant_propagation = function
  | Neg x -> (
    match constant_propagation x with Cst c -> Cst (Q.neg c) | x -> Neg x )
  | Binary ((ADD | SUB), e, Cst z) when z = Q.zero -> e
  | Binary ((ADD | SUB), Cst z, e) when z = Q.zero -> e
  | Binary (MUL, _, Cst z) when z = Q.zero -> zero
  | Binary (MUL, Cst z, _) when z = Q.zero -> zero
  | Binary ((MUL | DIV), e, Cst z) when z = Q.one -> e
  | Binary (MUL, Cst z, e) when z = Q.one -> e
  | Binary ((MUL | DIV), e, Cst z) when z = Q.minus_one -> Neg e
  | Binary (MUL, Cst z, e) when z = Q.minus_one -> Neg e
  | Binary (op, e1, e2) -> (
    match (constant_propagation e1, constant_propagation e2) with
    | Cst c1, Cst c2 ->
        let f = op_to_fun op in
        Cst (f c1 c2)
    | e1, e2 -> Binary (op, e1, e2) )
  | x -> x

let replace ?(simplify = true) e1 v value =
  let rec aux = function
    | Funcall (name, args) -> Funcall (name, List.map aux args)
    | Neg e -> aux e
    | Binary (b, e1, e2) -> Binary (b, aux e1, aux e2)
    | Var v' as var -> if v' = v then value else var
    | cst -> cst
  in
  let res = aux e1 in
  if simplify then constant_propagation res else res

let fix_var ?(simplify = true) (e : t) v (c : Q.t) : t =
  replace ~simplify e v (Cst c)

let check_arity_1 args = match args with [x] -> x | _ -> raise Arity_error

let check_arity_2 args =
  match args with [x; y] -> (x, y) | _ -> raise Arity_error

let function_env_1 =
  [ ("sqrt", sqrt)
  ; ("exp", exp)
  ; ("ln", log)
  ; ("cos", cos)
  ; ("sin", sin)
  ; ("tan", tan)
  ; ("acos", acos)
  ; ("asin", asin)
  ; ("atan", atan) ]

let apply fn args =
  match fn with
  | "abs" -> Q.abs (check_arity_1 args)
  | "max" ->
      let x, y = check_arity_2 args in
      if Q.compare x y > 0 then x else y
  | "min" ->
      let x, y = check_arity_2 args in
      if Q.compare x y > 0 then y else x
  (* potential rounding errors *)
  | _ -> (
    (* all remaining function are unary *)
    try
      let a = check_arity_1 args in
      Q.of_float ((List.assoc fn function_env_1) (Q.to_float a))
    with Not_found ->
      failwith (Format.sprintf "function %s not supported in Expr.eval" fn) )

let eval (e : t) (i : Instance.t) : Q.t =
  let rec aux = function
    | Funcall (name, args) -> apply name (List.map aux args)
    | Neg e -> Q.neg (aux e)
    | Binary (b, e1, e2) -> (
      match b with
      | ADD -> Q.add (aux e1) (aux e2)
      | SUB -> Q.sub (aux e1) (aux e2)
      | MUL -> Q.mul (aux e1) (aux e2)
      | DIV -> (
        match Q.div (aux e1) (aux e2) with
        | None -> raise Division_by_zero
        | Some x -> x )
      | POW -> (
        match Q.to_int (aux e2) with
        | None -> raise Non_integer_exposant
        | Some x -> Q.pow (aux e1) x ) )
    | Var v -> VarMap.find v i
    | Cst q -> q
  in
  aux e

let rec deannot = function
  | AFuncall (f, args) -> Funcall (f, List.map (fun (e, _a) -> deannot e) args)
  | ANeg (e, _) -> Neg (deannot e)
  | ABinary (b, (e1, _), (e2, _)) -> Binary (b, deannot e1, deannot e2)
  | AVar v -> Var v
  | ACst c -> Cst c

let deannot ((e, _a) : 'a annot_t) = deannot e

let pp_var = Format.pp_print_string

let pp_binop fmt = function
  | ADD -> Format.fprintf fmt "+"
  | SUB -> Format.fprintf fmt "-"
  | MUL -> Format.fprintf fmt "*"
  | DIV -> Format.fprintf fmt "/"
  | POW -> Format.fprintf fmt "^"

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

let to_string : t -> string = Format.asprintf "%a" print

let compare : t -> t -> int = compare

module Operators = struct
  let ( + ) = add

  let ( * ) = mul

  let ( - ) = sub

  let ( / ) = div
end
