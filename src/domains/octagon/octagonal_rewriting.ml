open Csp

type sign = Positive | Negative
let reverse_sign = function
| Positive -> Negative
| Negative -> Positive

type octagonal_constraint = {
  x: sign * Csp.var;
  y: sign * Csp.var;
  c: Bound_rat.t;
}

let vars_of oc =
  let (_, v1) = oc.x in
  let (_, v2) = oc.y in
  if String.equal v1 v2 then [v1] else [v1;v2]

let reverse_sign oc =
  let rev (s, v) = (reverse_sign s, v) in
  { oc with
    x=rev oc.x;
    y=rev oc.y;
  }

(** `x <= c` ~> `x + x <= 2c` *)
let x_leq_c x c = {x=(Positive, x); y=(Negative,x); c=(Bound_rat.mul_up c Bound_rat.two)}

(** `-x <= c` ~> `-x - x <= 2c` *)
let minus_x_leq_c x c = {x=(Negative, x); y=(Positive,x); c=(Bound_rat.mul_up c Bound_rat.two)}

(** `x + y <= c` *)
let x_plus_y_leq_c x y c = {x=(Positive, x); y=(Negative,y); c=c}

(** `x - y <= c` *)
let x_minus_y_leq_c x y c = {x=(Positive, x); y=(Positive,y); c=c}

(** `-x + y <= c` *)
let minus_x_plus_y_leq_c x y c = {x=(Negative, x); y=(Negative,y); c=c}

(** `-x - y <= c` *)
let minus_x_minus_y_leq_c x y c = {x=(Negative, x); y=(Positive,y); c=c}

let create_if_two_vars = function
  | Binary (ADD, Var x, Var y), LEQ, Cst (c, _) ->  Some (x_plus_y_leq_c x y c)
  | Binary (SUB, Var x, Var y), LEQ, Cst (c, _) ->  Some (x_minus_y_leq_c x y c)
  | Binary (ADD, Unary (NEG, Var x), Var y), LEQ, Cst (c, _) ->  Some (minus_x_plus_y_leq_c x y c)
  | Binary (SUB, Unary (NEG, Var x), Var y), LEQ, Cst (c, _) ->  Some (minus_x_minus_y_leq_c x y c)
  | _ -> None

let try_create = function
  | Var x, LEQ, Cst (c, _) -> Some (x_leq_c x c)
  | Unary (NEG, Var x), LEQ, Cst (c, _) -> Some (minus_x_leq_c x c)
  | c -> create_if_two_vars c

let rec normalize_expr e =
  let neg e = Unary (NEG, e) in
  match e with
  | Unary (NEG, Cst(c,a)) -> Cst (Bound_rat.neg c, a)
  | Unary (NEG, Unary (NEG, e)) -> normalize_expr e
  | Unary (NEG, Binary (SUB, x, y)) -> normalize_expr (Binary (ADD, neg x, y))
  | Unary (NEG, Binary (ADD, x, y)) -> normalize_expr (Binary (SUB, neg x, y))
  | Unary (op, x) -> Unary(op, normalize_expr x)
  | Binary (SUB, x, Unary (NEG, y)) -> normalize_expr (Binary (ADD, x, y))
  | Binary (ADD, x, Unary (NEG, y)) -> normalize_expr (Binary (SUB, x, y))
  | Binary (op, x, y) -> Binary (op, normalize_expr x, normalize_expr y)
  | e -> e

let normalize (e1, op, e2) = ((normalize_expr e1), op, (normalize_expr e2))

let rec generic_rewrite c =
  match normalize c with
  | e1, GEQ, e2 -> generic_rewrite (Unary (NEG, e1), LEQ, Unary (NEG, e2))
  | e1, GT, e2 -> generic_rewrite (Unary (NEG, e1), LT, Unary (NEG, e2))
  | Var x, op, Var y -> generic_rewrite (Binary (SUB, Var x, Var y), op, Cst (Bound_rat.zero, Int))
  | e1, EQ, e2 -> (generic_rewrite (e1, LEQ, e2))@(generic_rewrite (e1, GEQ, e2))
  | c -> [c]

let unwrap_all constraints =
  if List.for_all Tools.is_some constraints then
    List.map Tools.unwrap constraints
  else
    []

module type Rewriter_sig =
sig
  val rewrite: bconstraint -> octagonal_constraint list
  val relax: bconstraint -> octagonal_constraint list
end

module RewriterZ =
struct
(** It reformulates strict inequalities `<` into the inequality `<=` (dual `>` is handled by `rewrite`).
    Contrarily to `relax`, the rewritten constraint is logically equivalent to the initial constraint.
    For example, it rewrites `x - y < c` into `x - y <= c - 1`. *)
  let reformulate = function
  | e1, LT, Cst (c, a) -> normalize (e1, LEQ, Cst (Bound_rat.sub_up c Bound_rat.one, a))
  | c -> c

  let rewrite c =
    generic_rewrite c |>
    List.map (fun c -> try_create (reformulate c)) |>
    unwrap_all

  let relax c = []
end

module RewriterQF =
struct
  let rewrite c = generic_rewrite c |> List.map try_create |> unwrap_all

  (** Relax a constraint to turn it into an octagonal constraint.
      It rewrites strict inequalities `<` into the inequality `<=` (dual `>` must be handled by `rewrite`). *)
  let relax = function
  | e1, LT, e2 -> rewrite (e1, LEQ, e2)
  | c -> []
end

