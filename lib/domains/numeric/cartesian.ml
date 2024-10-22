open Tools
open Itv_sig
open Csp

(* Generic functor for a parametrized Cartesian representation *)
module Box (I : ITV) = struct
  (* maps each variable to a (non-empty) interval *)
  type cart = I.t VarMap.t

  type t = {support: (Csp.typ * string * Dom.t) list; ranges: cart}

  (* this domain uses the same language than the one defined in Csp.ml *)
  type internal_constr = Constraint.comparison

  let sat i c = Constraint.eval_comparison c i

  (* elem is not used but required by the interface. disabling the unused
     parameter warning to make dune happy *)
  let[@warning "-27"] internalize ?elem = Fun.id

  let externalize = Fun.id

  let vars (abs : t) : (Csp.typ * string * Dom.t) list = abs.support

  let is_representable _ = Kleene.True

  (* Printer *)
  let print fmt a =
    Format.fprintf fmt "{%a}"
      (VarMap.print
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
         (fun fmt -> Format.fprintf fmt "=%a" I.print) )
      a.ranges

  (* Set-theoretic *)

  (* NOTE: all binary operations assume that both elements are defined on the
     same set of variables; otherwise, an Invalid_argument exception will be
     raised *)

  let join (a : t) (b : t) : t * bool =
    let join_opt a b =
      match (a, b) with Some a, Some b -> Some (I.join a b) | _ -> None
    in
    ({a with ranges= VarMap.merge (fun _ -> join_opt) a.ranges b.ranges}, false)

  (* TODO: try to do better than this *)
  let join_list (l : t list) : t * bool =
    match l with
    | [] -> invalid_arg "Cartesian.join_list: empty list"
    | h :: t ->
        List.fold_left
          (fun (e, b) e' ->
            let e, b' = join e e' in
            (e, b && b') )
          (h, true) t

  let meet (a : t) (b : t) : t option =
    let meet_opt a b =
      match (a, b) with Some a, Some b -> Some (I.meet a b) | _ -> raise Exit
    in
    try Some {a with ranges= VarMap.merge (fun _ -> meet_opt) a.ranges b.ranges}
    with Exit | Bot_found -> None

  (* mesure *)
  (* ------ *)

  (* variable with maximal range *)
  let max_range (a : t) : string * float =
    let vo, io = VarMap.min_binding a.ranges in
    let so = I.float_size io in
    VarMap.fold
      (fun v i ((_, so) as acc) ->
        let si = I.float_size i in
        if si > so then (v, si) else acc )
      a.ranges (vo, so)

  (* variable with maximal range if real or with minimal if integer *)
  let mix_range (a : t) : string * I.t =
    VarMap.fold
      (fun v i (vo, io) -> if I.score i > I.score io then (v, i) else (vo, io))
      a.ranges
      (VarMap.min_binding a.ranges)

  let diff : (t -> t -> t list) option =
    let rec aux diff a acc = function
      | [] -> acc
      | (v, i_b) :: tl ->
          let add i = {a with ranges= VarMap.add v i a.ranges} in
          let i_a = VarMap.find v a.ranges in
          let d = diff i_a i_b in
          let rest = I.meet_opt i_a i_b |> Option.get in
          aux diff (add rest) (List.rev_append (List.rev_map add d) acc) tl
    in
    match I.prune with
    | None -> None
    | Some diff -> Some (fun a b -> aux diff a [] (VarMap.bindings b.ranges))

  let volume (a : t) : float =
    VarMap.fold (fun _ x v -> I.float_size x *. v) a.ranges 1.

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
          let r = VarMap.find_fail v a.ranges in
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
    | AVar v ->
        { a with
          ranges= VarMap.add v (I.meet x (VarMap.find_fail v a.ranges)) a.ranges
        }
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

  let rec refine_diff (a : t) (e : I.t Expr.annot) (x : I.t) : t * VarSet.t =
    let open Expr in
    match e with
    | AFuncall (name, args) ->
        let bexpr, itv = List.split args in
        let res = I.filter_fun name itv x in
        List.fold_left2
          (fun (acc, v) e1 e2 ->
            let a', v' = refine_diff acc e2 e1 in
            (a', VarSet.union v v') )
          (a, VarSet.empty) (Option.get res) bexpr
    | AVar v ->
        let old_i = VarMap.find_fail v a.ranges in
        let new_i = I.meet x old_i in
        if old_i = new_i then (a, VarSet.empty)
        else ({a with ranges= VarMap.add v new_i a.ranges}, VarSet.singleton v)
    | ACst i ->
        ignore (I.meet x (I.of_rat i)) ;
        (a, VarSet.empty)
    | ANeg (e1, i1) -> refine_diff a e1 (Option.get (I.filter_neg i1 x))
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
        let r1, v1 = refine_diff a e1 j1 in
        let r2, v2 = refine_diff r1 e2 j2 in
        (r2, VarSet.union v1 v2)

  (* test transfer function. Apply the evaluation followed by the refine step of
     the HC4-revise algorithm. It prunes the domain of the variables in `a`
     according to the constraint `e1 o e2`. *)
  let test_diff (a : t) (e1 : Expr.t) (o : Constraint.cmpop) (e2 : Expr.t) :
      (t * VarSet.t) Consistency.t =
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
      (fun (j1, j2) _b ->
        let r1, v1 = refine_diff a b1 j1 in
        let r2, v2 = refine_diff r1 b2 j2 in
        Filtered ((r2, VarSet.union v1 v2), false) )
      res

  let filter_diff (a : t) (e1, binop, e2) : (t * VarSet.t) Consistency.t =
    try test_diff a e1 binop e2
    with Invalid_argument _ | Bot_found -> Consistency.Unsat

  let empty : t = {support= []; ranges= VarMap.empty}

  let is_empty abs = VarMap.is_empty abs

  let add_var (abs : t) (typ, var, domain) : t =
    let open Dom in
    { abs with
      ranges=
        VarMap.add var
          ( match (typ, domain) with
          | Int, Finite (l, u) -> I.of_rats l u
          | Real, Finite (l, u) -> I.of_rats l u
          | _ -> failwith "cartesian.ml : add_var" )
          abs.ranges }

  let rm_var abs var : t = {abs with ranges= VarMap.remove var abs.ranges}

  let eval (abs : t) cons =
    let _, bounds = eval abs cons in
    I.to_rational_range bounds

  let to_constraint (a : t) : Constraint.t =
    match VarMap.bindings a.ranges with
    | [] -> assert false
    | (v, i) :: tl ->
        List.fold_left
          (fun acc (v, i) -> Constraint.And (acc, I.to_constraint v i))
          (I.to_constraint v i) tl

  (** {1 Sanity and checking functions} *)

  (* returns an randomly (uniformly?) chosen instanciation of the variables *)
  let spawn (a : t) : instance =
    VarMap.fold
      (fun v itv acc ->
        if VarMap.mem v acc then acc
        else
          let t, _, _dom = List.find (fun (_, v', _) -> v = v') a.support in
          let value = I.spawn itv in
          match t with
          | Csp.Real -> VarMap.add v (value |> Q.of_float) acc
          | Csp.Int -> VarMap.add v (floor value |> Q.of_float) acc )
      a.ranges VarMap.empty

  (* given an abstraction a and an instance i , verifies if i \in \gamma(a) *)
  let is_abstraction (a : t) (i : instance) =
    VarMap.for_all
      (fun k value ->
        let value = Q.to_float value in
        let itv = VarMap.find_fail k a.ranges in
        I.contains_float itv value )
      i

  (* split *)
  (* ----- *)

  let split_along ?prec (v : string) (a : t) : t list =
    ignore prec ;
    let i = VarMap.find v a.ranges in
    let i_list = I.split i in
    List.fold_left
      (fun acc b -> {a with ranges= VarMap.add v b a.ranges} :: acc)
      [] i_list

  let split ?prec (a : t) : t list =
    let v, si = max_range a in
    match prec with
    | None -> split_along v a
    | Some prec ->
        if si < prec then raise Signature.Too_small else split_along v a

  let split_diff ?prec (a : t) : t list * VarSet.t =
    let v, si = max_range a in
    match prec with
    | None -> (split_along v a, VarSet.singleton v)
    | Some prec ->
        if si < prec then raise Signature.Too_small
        else (split_along v a, VarSet.singleton v)

  let render (x : t) =
    let vars, values = VarMap.bindings x.ranges |> List.split in
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
