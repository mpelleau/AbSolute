open Tools

type cmpop = EQ | LEQ | GEQ | NEQ | GT | LT

type comparison = Expr.t * cmpop * Expr.t

type 'a boolean =
  | Cmp of 'a
  | And of 'a boolean * 'a boolean
  | Or of 'a boolean * 'a boolean
  | Not of 'a boolean

type t = comparison boolean

let leq e1 e2 : t = Cmp (e1, LEQ, e2)

let lt e1 e2 : t = Cmp (e1, LT, e2)

let geq e1 e2 : t = Cmp (e1, GEQ, e2)

let gt e1 e2 : t = Cmp (e1, GT, e2)

let eq e1 e2 : t = Cmp (e1, EQ, e2)

let neq e1 e2 : t = Cmp (e1, NEQ, e2)

let and_ b1 b2 : t = And (b1, b2)

let or_ b1 b2 : t = Or (b1, b2)

let not_ b : t = Not b

let imply b1 b2 = Or (Not b1, b2)

let assign var value : t = eq (Expr.var var) (Expr.of_mpqf value)

let inside v low high : t = And (geq v low, leq v high)

let outside v low high : t = Or (lt v low, gt v high)

let inside_cst v l h : t = Expr.(inside (var v) (of_mpqf l) (of_mpqf h))

let outside_cst v l h : t = Expr.(outside (var v) (of_mpqf l) (of_mpqf h))

let of_instance (i : Instance.t) =
  match VarMap.bindings i with
  | [] -> invalid_arg "Constraint.of_instance: empty instance"
  | (v, q) :: tl ->
      List.fold_left
        (fun acc (v', q') -> and_ acc (assign v' q'))
        (assign v q) tl

let of_apron_lincons lc =
  let open Apronext in
  let c_to_q c = c |> Coeffext.to_mpqf |> Expr.of_mpqf in
  let res =
    Linconsext.fold
      (fun c v -> Expr.(add (mul (c_to_q c) (var (Apron.Var.to_string v)))))
      c_to_q lc
  in
  let cmp =
    match Linconsext.get_typ lc with
    | SUPEQ -> geq
    | SUP -> gt
    | EQ -> eq
    | _ -> assert false
  in
  cmp res Expr.zero

let convex_hull =
  let open Apronext in
  fun (instances : Instance.t list) : t ->
    let g_l = List.map Instance.to_apron_gen instances in
    let pol = Apol.of_generator_list g_l in
    match Apol.to_lincons_list pol with
    | [] -> assert false
    | h :: tl ->
        List.fold_left
          (fun acc c -> And (acc, of_apron_lincons c))
          (of_apron_lincons h) tl

let inv_cmp = function
  | EQ -> EQ
  | LEQ -> GEQ
  | GEQ -> LEQ
  | NEQ -> NEQ
  | GT -> LT
  | LT -> GT

let neg_cmp = function
  | EQ -> NEQ
  | LEQ -> GT
  | GEQ -> LT
  | NEQ -> EQ
  | GT -> LEQ
  | LT -> GEQ

let cmp_to_fun cmpop a b =
  let cmp = Q.compare a b in
  match cmpop with
  | EQ -> cmp = 0
  | LEQ -> cmp <= 0
  | GEQ -> cmp >= 0
  | NEQ -> cmp <> 0
  | GT -> cmp > 0
  | LT -> cmp < 0

let nullify_rhs ((e1, c, e2) : comparison) : comparison =
  (Expr.sub e1 e2, c, Expr.zero)

let rec neg : t -> t = function
  | Cmp (e1, op, e2) -> Cmp (e1, neg_cmp op, e2)
  | And (b1, b2) -> Or (neg b1, neg b2)
  | Or (b1, b2) -> And (neg b1, neg b2)
  | Not b -> b

let rec remove_not : t -> t = function
  | Not b -> remove_not (neg b)
  | And (b1, b2) -> And (remove_not b1, remove_not b2)
  | Or (b1, b2) -> Or (remove_not b1, remove_not b2)
  | x -> x

let rec collect_vars =
  let merge = VarMap.union (fun _v i1 i2 -> Some (i1 + i2)) in
  function
  | Not b -> collect_vars (neg b)
  | And (b1, b2) | Or (b1, b2) -> merge (collect_vars b1) (collect_vars b2)
  | Cmp (e1, _, e2) -> merge (Expr.collect_vars e1) (Expr.collect_vars e2)

let replace (constr : t) v c : t =
  let rec aux = function
    | Cmp (e1, cmp, e2) -> Cmp (Expr.replace e1 v c, cmp, Expr.replace e2 v c)
    | And (c1, c2) -> And (aux c1, aux c2)
    | Or (c1, c2) -> Or (aux c1, aux c2)
    | Not c -> Not (aux c)
  in
  aux constr

let fix_var constr v (c : Q.t) : t = replace constr v (Cst c)

let eval (constr : t) i =
  let rec aux = function
    | Not b -> not (aux b)
    | And (b1, b2) -> aux b1 && aux b2
    | Or (b1, b2) -> aux b1 || aux b2
    | Cmp (e1, op, e2) -> (cmp_to_fun op) (Expr.eval e1 i) (Expr.eval e2 i)
  in
  aux constr

let pp_cmpop fmt = function
  | NEQ -> Format.fprintf fmt "<>"
  | LEQ -> Format.fprintf fmt "<="
  | GEQ -> Format.fprintf fmt ">="
  | EQ -> Format.fprintf fmt "="
  | LT -> Format.fprintf fmt "<"
  | GT -> Format.fprintf fmt ">"

let pp_comparison fmt ((e1, c, e2) : comparison) =
  Format.fprintf fmt "%a %a %a" Expr.print e1 pp_cmpop c Expr.print e2

let rec print fmt : t -> unit = function
  | Cmp c -> pp_comparison fmt c
  | And (b1, b2) -> Format.fprintf fmt "%a && %a" print b1 print b2
  | Or (b1, b2) -> Format.fprintf fmt "%a || %a" print b1 print b2
  | Not b -> Format.fprintf fmt "not %a" print b

let to_string : t -> string = Format.asprintf "%a" print
