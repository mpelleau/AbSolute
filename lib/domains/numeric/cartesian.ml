open Tools
open Itv_sig
open Csp

(* Generic functor for a parametrized Cartesian representation *)
module Box (I : ITV) = struct
  (* maps from variables *)
  module Env = Tools.VarMap

  (* maps each variable to a (non-empty) interval *)
  type t = I.t Env.t

  (* this domain uses the same language than the one defined in Csp.ml *)
  type internal_constr = Constraint.comparison

  (* elem is not used but required by the interface. disabling the unused
     parameter warning to make merlin happy *)
  let[@warning "-27"] internalize ?elem = Fun.id

  let externalize = Fun.id

  (* same as find but prints the key before failing *)
  let find = VarMap.find_fail

  (* true if 'var' is an integer in the given environment *)
  let is_integer var abs = I.to_annot (VarMap.find abs var) = Csp.Int

  let vars abs =
    Env.fold
      (fun v _ acc ->
        let typ = if is_integer abs v then Int else Real in
        (typ, v) :: acc)
      abs []

  let is_representable _ = Kleene.True

  (* Printer *)
  let print fmt = VarMap.iter (fun v -> Format.fprintf fmt "%s:%a\n" v I.print)

  (* Set-theoretic *)

  (* NOTE: all binary operations assume that both elements are defined on the
     same set of variables; otherwise, an Invalid_argument exception will be
     raised *)

  let join (a : t) (b : t) : t * bool =
    let join_opt a b =
      match (a, b) with Some a, Some b -> Some (I.join a b) | _ -> None
    in
    (VarMap.merge (fun _ -> join_opt) a b, false)

  (* TODO: try to do better than this *)
  let join_list (l : t list) : t * bool =
    match l with
    | [] -> invalid_arg "Cartesian.join_list: empty list"
    | h :: t ->
        List.fold_left
          (fun (e, b) e' ->
            let e, b' = join e e' in
            (e, b && b'))
          (h, true) t

  let meet (a : t) (b : t) : t option =
    let meet_opt a b =
      match (a, b) with Some a, Some b -> Some (I.meet a b) | _ -> raise Exit
    in
    try Some (VarMap.merge (fun _ -> meet_opt) a b)
    with Exit | Bot_found -> None

  (* mesure *)
  (* ------ *)

  (* variable with maximal range *)
  let max_range (a : t) : string * I.t =
    VarMap.fold
      (fun v i (vo, io) ->
        if I.float_size i > I.float_size io then (v, i) else (vo, io))
      a (VarMap.min_binding a)

  (* variable with maximal range if real or with minimal if integer *)
  let mix_range (a : t) : string * I.t =
    VarMap.fold
      (fun v i (vo, io) -> if I.score i > I.score io then (v, i) else (vo, io))
      a (VarMap.min_binding a)

  let diff =
    let rec aux diff a acc = function
      | [] -> acc
      | (v, i_b) :: tl ->
          let add i = Env.add v i a in
          let i_a = Env.find v a in
          let d = diff i_a i_b in
          let rest = I.meet_opt i_a i_b |> Option.get in
          aux diff (add rest) (List.rev_append (List.rev_map add d) acc) tl
    in
    match I.prune with
    | None -> None
    | Some diff -> Some (fun a b -> aux diff a [] (Env.bindings b))

  let volume (a : t) : float =
    VarMap.fold (fun _ x v -> I.float_size x *. v) a 1.

  (* splitting strategies *)

  let choose a = mix_range a

  (************************************************************************)
  (* ABSTRACT OPERATIONS *)
  (************************************************************************)

  (* trees with nodes annotated with evaluation *)
  type bexpri = I.t Expr.annot_t

  (* First step of the HC4-revise algorithm: it computes the intervals for each
     node of the expression. For example: given `x + 3` with `x in [1..3]`, then
     it annotates `+` with `[4..6]`. It returns this new annotated expression
     tree, and the interval of the root node.

     This function is useful for testing transfer functions errors (e.g.
     division by zero). - We raise Bot_found in case the expression only
     evaluates to error values. - Otherwise, we return only the non-error
     values. *)
  let eval (a : t) (e : Expr.t) : bexpri =
    let open Expr in
    let rec loop = function
      | Funcall (name, args) ->
          let bargs = List.map loop args in
          let iargs = List.map snd bargs in
          let r = Option.get (I.eval_fun name iargs) in
          (AFuncall (name, bargs), r)
      | Var v ->
          let r = find v a in
          (AVar v, r)
      | Cst c ->
          let r = I.of_rat c in
          (ACst c, r)
      | Neg e1 ->
          let ((_, i1) as b1) = loop e1 in
          (ANeg b1, I.neg i1)
      | Binary (o, e1, e2) ->
          let ((_, i1) as b1) = loop e1 and ((_, i2) as b2) = loop e2 in
          let r =
            match o with
            | ADD -> I.add i1 i2
            | SUB -> I.sub i1 i2
            | DIV -> Option.get (I.div i1 i2)
            | MUL ->
                let r = I.mul i1 i2 in
                if e1 = e2 then (* special case: squares are positive *)
                  I.abs r
                else r
            | POW -> I.pow i1 i2
          in
          (ABinary (o, b1, b2), r)
    in
    loop e

  (* Second step of the HC4-revise algorithm. It propagates the intervals from
     the root of the expression tree `e` to the leaves. For example: Given `y =
     x + 3`, `x in [1..3]`, `y in [1..5]`. Then after `eval` we know that the
     node at `+` has the interval `[4..6]`. Therefore we can intersect `y` with
     `[4..6]` due to the equality. Note that we can call again `eval` to
     restrain further `+`, and then another round of `refine` will restrain `x`
     as well. We raise `Bot_found` in case of unsatisfiability. *)

  let rec refine (a : t) (e : I.t Expr.annot) (x : I.t) : t =
    let open Expr in
    match e with
    | AFuncall (name, args) ->
        let bexpr, itv = List.split args in
        let res = I.filter_fun name itv x in
        List.fold_left2
          (fun acc e1 e2 -> refine acc e2 e1)
          a (Option.get res) bexpr
    | AVar v -> Env.add v (I.meet x (find v a)) a
    | ACst i ->
        ignore (I.meet x (I.of_rat i)) ;
        a
    | ANeg (e1, i1) -> refine a e1 (Option.get (I.filter_neg i1 x))
    | ABinary (o, (e1, i1), (e2, i2)) ->
        let j =
          match o with
          | ADD -> I.filter_add i1 i2 x
          | SUB -> I.filter_sub i1 i2 x
          | MUL -> I.filter_mul i1 i2 x
          | DIV -> I.filter_div i1 i2 x
          | POW -> I.filter_pow i1 i2 x
        in
        let j1, j2 = Option.get j in
        refine (refine a e1 j1) e2 j2

  (* test transfer function. Apply the evaluation followed by the refine step of
     the HC4-revise algorithm. It prunes the domain of the variables in `a`
     according to the constraint `e1 o e2`. *)
  let test (a : t) (e1 : Expr.t) (o : Constraint.cmpop) (e2 : Expr.t) :
      t Consistency.t =
    let (b1, i1), (b2, i2) = (eval a e1, eval a e2) in
    let res =
      match o with
      | LT -> I.filter_lt i1 i2
      | LEQ -> I.filter_leq i1 i2
      (* a > b <=> b < a*)
      | GEQ -> Consistency.map swap_pair (I.filter_leq i2 i1)
      | GT -> Consistency.map swap_pair (I.filter_lt i2 i1)
      | NEQ -> I.filter_neq i1 i2
      | EQ -> Consistency.map (fun x -> (x, x)) (I.filter_eq i1 i2)
    in
    Consistency.bind
      (fun (j1, j2) _b -> Filtered (refine (refine a b1 j1) b2 j2, false))
      res

  let filter (a : t) (e1, binop, e2) : t Consistency.t =
    try test a e1 binop e2
    with Invalid_argument _ | Bot_found -> Consistency.Unsat

  let empty : t = Env.empty

  let is_empty abs = Env.is_empty abs

  let add_var (abs : t) (typ, var, domain) : t =
    let open Dom in
    VarMap.add var
      ( match (typ, domain) with
      | Int, Finite (l, u) -> I.of_rats l u
      | Real, Finite (l, u) -> I.of_rats l u
      | _ -> failwith "cartesian.ml : add_var" )
      abs

  let rm_var abs var : t = Env.remove var abs

  let forward_eval abs cons =
    let _, bounds = eval abs cons in
    I.to_rational_range bounds

  (* check if sound *)

  let to_constraint (a : t) : Constraint.t =
    match VarMap.bindings a with
    | [] -> assert false
    | (v, i) :: tl ->
        List.fold_left
          (fun acc (v, i) -> Constraint.And (acc, I.to_constraint v i))
          (I.to_constraint v i) tl

  (** {1 Sanity and checking functions} *)

  (* returns an randomly (uniformly?) chosen instanciation of the variables *)
  let spawn (a : t) : instance =
    VarMap.(fold (fun k itv -> add k (Q.of_float (I.spawn itv))) a empty)

  (* given an abstraction and instance, verifies if the abstraction is implied
     by the instance *)
  let is_abstraction (a : t) (i : instance) =
    VarMap.for_all
      (fun k value ->
        let value = Mpqf.to_float value in
        let itv = VarMap.find_fail k a in
        I.contains_float itv value)
      i

  (* split *)
  (* ----- *)

  let split_along (a : t) (v : string) : t list =
    let i = VarMap.find v a in
    let i_list = I.split i in
    List.fold_left (fun acc b -> VarMap.add v b a :: acc) [] i_list

  let split prec (a : t) : t list =
    let v, i = max_range a in
    if I.float_size i < prec then raise Signature.TooSmall else split_along a v

  let render x =
    let vars, values = VarMap.bindings x |> List.split in
    Picasso.Drawable.of_ranges vars (List.map I.to_float_range values)
end

(*************)
(* INSTANCES *)
(*************)

module BoxF = Box (Trigo.Make (Itv.ItvF))
module BoxStrict = Box (Trigo.Make (Newitv.Test))
module BoxQ = Box (Trigo.Make (Itv.ItvQ))
module BoxQStrict = Box (Trigo.Make (Newitv.TestQ))
module BoxMix = Box (Trigo.Make (Itv_mix))
