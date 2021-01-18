open Csp
open Tools
open Apron
open Apronext

let itv_to_mpqf i = Interval.(Scalarext.(to_mpqf i.inf, to_mpqf i.sup))

let scalar_mul_sqrt2 =
  let sqrt2_mpqf = Mpqf.of_float 0.707106781186548 in
  fun sca ->
    let value = Scalarext.to_mpqf sca in
    let mult = Mpqf.mul value sqrt2_mpqf in
    Scalar.of_mpqf mult

type point = float array

(* Compute the square of the euclidian distance between two points. *)
let sq_dist p1 p2 =
  let sum = ref 0. in
  Array.iter2 (fun a b -> sum := !sum +. ((a -. b) ** 2.)) p1 p2 ;
  !sum

(* compute the most distant pair of points of an array of points, and the
   corresponding distance*)
let most_distant_pair (pts : point array) : point * point * float =
  fold_on_combination_2
    (fun ((_, _, dist) as old) p1 p2 ->
      let dist' = sq_dist p1 p2 in
      if dist' > dist then (p1, p2, dist') else old)
    (pts.(0), pts.(0), 0.)
    pts

module type ADomain = sig
  type t

  val manager_alloc : unit -> t Manager.t
end

(* Translation functor for csp.prog to apron values*)
module SyntaxTranslator (D : ADomain) = struct
  type internal_constr = Tcons1.t

  let man = D.manager_alloc ()

  let of_expr env (e : Expr.t) : Texpr1.t =
    let open Expr in
    let rec aux = function
      | Funcall (name, args) -> (
        match (name, args) with
        | "sqrt", [x] -> Texprext.sqrt (aux x)
        | name, _ -> fail_fmt "%s not supported by apron" name )
      | Var v -> Texprext.var env (Var.of_string v)
      | Cst c -> Texprext.cst env (Coeff.s_of_mpqf c)
      | Neg e -> Texprext.neg (aux e)
      | Binary (o, e1, e2) ->
          ( match o with
          | ADD -> Texprext.add
          | SUB -> Texprext.sub
          | DIV -> Texprext.div
          | MUL -> Texprext.mul
          | POW -> Texprext.pow )
            (aux e1) (aux e2)
    in
    aux e

  let of_cmp_expr elem (e1, op, e2) =
    let env = Abstract1.env elem in
    let open Constraint in
    let e1 = of_expr env e1 and e2 = of_expr env e2 in
    match op with
    | EQ -> Tconsext.eq e1 e2
    | NEQ -> Tconsext.diseq e1 e2
    | GEQ -> Tconsext.geq e1 e2
    | GT -> Tconsext.gt e1 e2
    | LEQ -> Tconsext.leq e1 e2
    | LT -> Tconsext.lt e1 e2

  let apron_to_var abs =
    let env = Abstract1.env abs in
    let iv, rv = Environment.vars env in
    let ivars = Array.map (fun v -> Var.to_string v) iv in
    let rvars = Array.map (fun v -> Var.to_string v) rv in
    (Array.to_list ivars, Array.to_list rvars)

  let rec to_expr = function
    | Texpr1.Cst c -> Expr.of_mpqf (Coeffext.to_mpqf c)
    | Texpr1.Var v -> Expr.var (Var.to_string v)
    | Texpr1.Unop (Texpr1.Sqrt, e, _, _) -> Expr.Funcall ("sqrt", [to_expr e])
    | Texpr1.Unop (Texpr1.Neg, e, _, _) -> Expr.Neg (to_expr e)
    | Texpr1.Unop (Texpr1.Cast, _, _, _) -> failwith "cast should not occur"
    | Texpr1.Binop (op, e1, e2, _, _) ->
        let o =
          match op with
          | Texpr1.Add -> Expr.ADD
          | Texpr1.Sub -> Expr.SUB
          | Texpr1.Mul -> Expr.MUL
          | Texpr1.Div -> Expr.DIV
          | Texpr1.Mod -> failwith "Mod not yet supported with AbSolute"
          | _ -> failwith "operation not yet supported with AbSolute"
        in
        Binary (o, to_expr e1, to_expr e2)

  let apron_to_bexpr tcons =
    let apron_to_cmp op =
      match op with
      | Tcons1.EQ -> Constraint.EQ
      | Tcons1.DISEQ -> Constraint.NEQ
      | Tcons1.SUPEQ -> Constraint.GEQ
      | Tcons1.SUP -> Constraint.GT
      | _ -> failwith "operation not yet supported with AbSolute"
    in
    let typ = apron_to_cmp (Tcons1.get_typ tcons) in
    let exp = to_expr (Texpr1.to_expr (Tcons1.get_texpr1 tcons)) in
    (exp, typ, Expr.zero)

  let to_constraint abs : Constraint.t =
    let cons = Abstractext.to_tcons_array man abs in
    let l = Tcons1.array_length cons in
    let rec loop acc i =
      if i = l then acc
      else
        loop
          (Constraint.And (acc, Cmp (apron_to_bexpr (Tcons1.array_get cons i))))
          (i + 1)
    in
    loop (Constraint.Cmp (Tcons1.array_get cons 0 |> apron_to_bexpr)) 1
end

(*****************************************************************)
(* Some types and values that all the domains of apron can share *)
(* These are generic and can be redefined in the actuals domains *)
(*****************************************************************)
module MAKE (AP : ADomain) = struct
  module A = Abstractext
  module E = Environmentext

  type t = AP.t A.t

  include SyntaxTranslator (AP)

  let empty = A.top man E.empty

  let internalize ?elem c =
    match elem with
    | None ->
        invalid_arg
          "apron constraint conversion requires an abstract element to setup \
           the environment"
    | Some e -> of_cmp_expr e c

  let externalize = apron_to_bexpr

  let vars abs =
    let iv, rv = E.vars (A.env abs) in
    let iv = Array.fold_left (fun a v -> (Int, Var.to_string v) :: a) [] iv in
    Array.fold_left (fun a v -> (Real, Var.to_string v) :: a) iv rv

  let dom_to_texpr env =
    let open Dom in
    function
    | Top -> assert false
    | Finite (b1, b2) -> Texpr1.cst env (Coeff.i_of_mpqf b1 b2)
    | Minf _ -> assert false
    | Inf _ -> assert false
    | Set _ -> assert false

  let add_var abs (typ, v, dom) =
    let e = A.env abs in
    let v = Var.of_string v in
    let ints, reals = if typ = Int then ([|v|], [||]) else ([||], [|v|]) in
    let env = E.add e ints reals in
    let abs = A.change_environment man abs env false in
    let texpr = dom_to_texpr env dom in
    A.assign_texpr man abs v texpr None

  let var_bounds abs v =
    let var = Var.of_string v in
    let i = A.bound_variable man abs var in
    itv_to_mpqf i

  let rm_var abs v =
    let var = Var.of_string v in
    let e = E.remove (A.env abs) (Array.of_list [var]) in
    A.change_environment man abs e false

  let is_empty a = A.is_bottom man a

  let join a b = (A.join man a b, false)

  let meet a b =
    let m = A.meet man a b in
    if is_empty m then None else Some m

  let diff = None

  let filter abs c =
    let a = A.filter_tcons man abs c in
    if is_empty a then Consistency.Unsat
    else
      let succ = is_empty (A.filter_tcons man a (Tconsext.neg c)) in
      Consistency.Filtered (a, succ)

  let print = A.print

  let pman = Polka.manager_alloc_strict ()

  (** computes the smallest enclosing polyhedron *)
  let to_poly abs env =
    let abs' = A.change_environment man abs env false in
    A.to_lincons_array man abs' |> A.of_lincons_array pman env

  (** interval evaluation of an expression within an abtract domain *)
  let forward_eval abs cons =
    let ap_expr = of_expr (A.env abs) cons in
    A.bound_texpr man abs ap_expr |> itv_to_mpqf

  (* Given `largest abs = (v, i, d)`, `largest` extracts the variable `v` from
     `abs` * with the largest interval `i` = [l, u], and `d` the dimension of
     the * interval (`u - l` with appropriate rounding). *)
  let largest abs : Var.t * Interval.t * Q.t =
    let env = A.env abs in
    let box = A.to_box man abs in
    let tab = box.A.interval_array in
    let rec aux cur i_max diam_max itv_max =
      if cur >= Array.length tab then (i_max, diam_max, itv_max)
      else
        let e = tab.(cur) in
        let diam = Intervalext.range_mpqf e in
        if Mpqf.cmp diam diam_max > 0 then aux (cur + 1) cur diam e
        else aux (cur + 1) i_max diam_max itv_max
    in
    let a, b, c = aux 0 0 Q.zero tab.(0) in
    (E.var_of_dim env a, c, b)

  (* Compute the minimal and the maximal diameter of an array on intervals *)
  let rec minmax tab i max i_max min =
    if i >= Array.length tab then (Scalar.of_mpqf max, i_max, Scalar.of_mpqf min)
    else
      let dim = Intervalext.range_mpqf tab.(i) in
      if Mpqf.cmp dim max > 0 then minmax tab (i + 1) dim i min
      else if Mpqf.cmp min dim > 0 then minmax tab (i + 1) max i_max dim
      else minmax tab (i + 1) max i_max min

  (* let p1 = (p11, p12, ..., p1n) and p2 = (p21, p22, ..., p2n) two points
   * The vector p1p2 is (p21-p11, p22-p12, ..., p2n-p1n) and the orthogonal line
   * to the vector p1p2 passing by the center of the vector has for equation:
   * (p21-p11)(x1-b1) + (p22-p12)(x2-b2) + ... + (p2n-p1n)(xn-bn) = 0
   * with b = ((p11+p21)/2, (p12+p22)/2, ..., (p1n+p2n)/2) *)
  let generate_linexpr env p1 p2 =
    let size = E.size env in
    let rec loop i l1 l2 cst =
      if i >= size then (l1, l2, cst)
      else
        let ci = p2.(i) -. p1.(i) in
        let cst' = cst +. ((p1.(i) +. p2.(i)) *. ci) in
        let ci' = 2. *. ci in
        let c = Coeff.s_of_float ci' in
        let list1' = List.append l1 [(c, E.var_of_dim env i)] in
        let list2' = List.append l2 [(Coeff.neg c, E.var_of_dim env i)] in
        loop (i + 1) list1' list2' cst'
    in
    loop 0 [] [] 0.

  let split abs (e1, e2) =
    let meet_linexpr abs expr =
      let cons = Linconsext.make expr Lincons1.SUPEQ in
      A.filter_lincons man abs cons
    in
    let abs1 = meet_linexpr abs e1 in
    let abs2 = meet_linexpr abs e2 in
    [abs1; abs2]

  let volume abs =
    let b = A.to_box man abs in
    b.A.interval_array
    |> Array.fold_left (fun v i -> Q.mul v (Intervalext.range_mpqf i)) Q.one
    |> Q.to_float

  (* Polyhedric version of some operations *)
  let get_expr prec (polyad : Polka.strict Polka.t A.t) =
    let poly = A.to_generator_array pman polyad in
    let env = poly.Generator1.array_env in
    let p1, p2, dist = Generatorext.to_float_array poly |> most_distant_pair in
    if dist <= prec then raise Signature.TooSmall
    else
      let list1, list2, cst = generate_linexpr env p1 p2 in
      let linexp = Linexpr1.make env in
      Linexpr1.set_list linexp list1 (Some (Coeff.s_of_float (-.cst))) ;
      let linexp' = Linexpr1.make env in
      Linexpr1.set_list linexp' list2 (Some (Coeff.s_of_float cst)) ;
      (linexp, linexp')

  (* Sanity checking functions *)

  (** given an abstraction and instance, verifies if the abstraction is implied
      by the instance *)
  let is_abstraction poly instance =
    let env = A.env poly in
    let var, texpr =
      VarMap.fold
        (fun var value (acc1, acc2) ->
          let var = Apron.Var.of_string var in
          let value = Texpr1.cst env (Coeff.s_of_mpqf value) in
          (var :: acc1, value :: acc2))
        instance ([], [])
    in
    let var = Array.of_list var and tar = Array.of_list texpr in
    let poly_subst = A.substitute_texpr_array man poly var tar None in
    A.is_top man poly_subst

  (** Random uniform value within an interval, according to the type *)
  let spawn_itv typ (i : Interval.t) =
    let inf, sup = itv_to_mpqf i in
    match typ with
    | Environment.INT ->
        let size = Mpqf.sub sup inf |> Q.ceil in
        let r = Q.of_int (Random.int (size + 1)) in
        Q.add inf r
    | Environment.REAL ->
        let r = Q.of_float (Random.float 1.) in
        Q.add inf (Q.mul (Mpqf.sub sup inf) r)

  (** spawns an instance within a box *)
  let spawn_box box =
    let env = box.A.box1_env in
    let itvs = box.A.interval_array in
    let instance, _ =
      Array.fold_left
        (fun (acc, idx) i ->
          let v = E.var_of_dim env idx in
          let typ = E.typ_of_var env v in
          let instance = VarMap.add (Var.to_string v) (spawn_itv typ i) acc in
          (instance, idx + 1))
        (VarMap.empty, 0) itvs
    in
    instance

  (** Takes an integer and compute a spawner function. The integer * corresponds
      to the number of tries allowed to proceed to the * generation. The bigger
      it is, the more uniform the spawner will be. A * spawner returns a
      randomly uniformly chosen instanciation of the * variables. if the
      polyhedron has a nul (or very small) volume, (e.g * equalities in the
      constraints) uniformity is not guaranteed *)
  let spawner (nb_try : int) poly =
    let env = A.env poly in
    let rec retry poly n idx =
      let b = A.to_box man poly in
      let instance = spawn_box b in
      if is_abstraction poly instance then instance
      else if n >= nb_try then (
        (* in case we didnt find an instance, we fix a variable and retry. we
           give up on uniformity to enforce termination *)
        let v = E.var_of_dim env idx in
        let typ = E.typ_of_var env v in
        let v_itv = A.bound_variable man poly v in
        let v = Texpr1.var env (E.var_of_dim env idx) in
        let value = Texpr1.cst env (Coeff.s_of_mpqf (spawn_itv typ v_itv)) in
        let texpr = Texprext.sub v value in
        let tcons = Tcons1.make texpr Tcons1.EQ in
        let tearray = Tcons1.array_make env 1 in
        Tcons1.array_set tearray 0 tcons ;
        let poly = A.meet_tcons_array man poly tearray in
        retry poly 0 (idx + 1) )
      else retry poly (n + 1) idx
    in
    retry poly 0 0

  let spawn = spawner 10

  let render abs = Picasso.Drawable.of_pol (to_poly abs A.(abs.env))
end
