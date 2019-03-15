open Csp
open Dbm

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

module type Rewriter_sig = functor (B: Bound_sig.BOUND) ->
sig
  module B: Bound_sig.BOUND
  type t

  val init: (var * dbm_interval) list -> t
  val var_dbm_to_box: t -> dbm_interval -> var
  val var_box_to_dbm: t -> var -> dbm_interval
  val rewrite: t -> bconstraint -> (B.t dbm_constraint) list
  val relax: t -> bconstraint -> (B.t dbm_constraint) list
end with module B=B

module Rewriter(B: Bound_sig.BOUND) =
struct
  module B = B
  module Env = Tools.VarMap
  module REnv = Mapext.Make(struct
    type t=dbm_interval
    let compare = compare end)

  type t = {
    (* maps each variable name to its DBM interval. *)
    env: dbm_interval Env.t;
    (* reversed mapping of `env`. *)
    renv: var REnv.t;
  }

  let empty = {env=Env.empty; renv=REnv.empty}
  let add_var rewriter (v,itv) = {
    env=(Env.add v itv rewriter.env);
    renv=(REnv.add itv v rewriter.renv);
  }

  let init vars = List.fold_left add_var empty vars

  let var_dbm_to_box rewriter itv = REnv.find itv rewriter.renv
  let var_box_to_dbm rewriter v = Env.find v rewriter.env

  let dim_of_var rewriter v =
    let itv = var_box_to_dbm rewriter v in
    let k1, k2 = (itv.lb.x / 2), (itv.lb.y / 2) in
    if k1 <> k2 then failwith "Rewriter.dim_of_var: only variable with a canonical plane are defined on a single dimension."
    else k1

  (** If the bound is discrete, it reformulates strict inequalities `<` into the inequality `<=` (dual `>` is handled by `rewrite`).
      Contrarily to `relax`, the rewritten constraint is logically equivalent to the initial constraint.
      For example, it rewrites `x - y < c` into `x - y <= c - 1`. *)
  let reformulate c =
    if B.is_continuous then c
    else
      match c with
      | e1, LT, Cst (c, a) -> normalize (e1, LEQ, Cst (Bound_rat.sub_up c Bound_rat.one, a))
      | c -> c

  (** `x <= c` ~> `x + x <= 2c` *)
  let x_leq_c x c = {v={x=x;y=x+1}; c=(B.mul_up c B.two)}

  (** `-x <= c` ~> `-x - x <= 2c` *)
  let minus_x_leq_c x c = {v={x=x+1;y=x}; c=(B.mul_up c B.two)}

  (** `x + y <= c` *)
  let x_plus_y_leq_c x y c = {v={x=x;y=y+1}; c=c}

  (** `x - y <= c` *)
  let x_minus_y_leq_c x y c = {v={x=x;y=y}; c=c}

  (** `-x + y <= c` *)
  let minus_x_plus_y_leq_c x y c = {v={x=x+1; y=y+1}; c=c}

  (** `-x - y <= c` *)
  let minus_x_minus_y_leq_c x y c = {v={x=x+1; y=y}; c=c}

  let map_to_dim r f x c = f ((dim_of_var r x)*2) (B.of_rat_up c)
  let map2_to_dim r f x y c = f ((dim_of_var r x)*2) ((dim_of_var r y)*2) (B.of_rat_up c)

  let try_create r = function
    | Var x, LEQ, Cst (c, _) -> Some (map_to_dim r x_leq_c x c)
    | Unary (NEG, Var x), LEQ, Cst (c, _) -> Some (map_to_dim r minus_x_leq_c x c)
    | Binary (ADD, Var x, Var y), LEQ, Cst (c, _) ->  Some (map2_to_dim r x_plus_y_leq_c x y c)
    | Binary (SUB, Var x, Var y), LEQ, Cst (c, _) ->  Some (map2_to_dim r x_minus_y_leq_c x y c)
    | Binary (ADD, Unary (NEG, Var x), Var y), LEQ, Cst (c, _) ->  Some (map2_to_dim r minus_x_plus_y_leq_c x y c)
    | Binary (SUB, Unary (NEG, Var x), Var y), LEQ, Cst (c, _) ->  Some (map2_to_dim r minus_x_minus_y_leq_c x y c)
    | _ -> None

  let unwrap_all constraints =
    if List.for_all Tools.is_some constraints then
      List.map Tools.unwrap constraints
    else
      []

  let rewrite r c =
    generic_rewrite c |>
    List.map (fun c -> try_create r (reformulate c)) |>
    unwrap_all

  let relax r c =
    List.flatten (List.map (fun c ->
      match c with
      | e1, LT, e2 -> rewrite r (e1, LEQ, e2)
      | c -> []
    ) (generic_rewrite c))
end

