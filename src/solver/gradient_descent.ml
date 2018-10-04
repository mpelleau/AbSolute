open VectorMap

(**
 * Type of gradients: a map associating variables to a partial derivative.
 *)
type gradient = P.Float.t Tools.VarMap.t

(**
 * Computes an iteration of the gradient descient.
 *)
let next_point : float -> gradient -> FloatVec.t -> FloatVec.t
    = fun gamma gradient point ->
    FloatVec.sub
        point
        (FloatVec.mulc
            gamma
            (P.Float.eval_gradient gradient point)
        )

(**
 * Iterates the gradient descent.
 *)
let rec descent : (FloatVec.t -> bool) -> float -> float -> FloatVec.t -> gradient -> FloatVec.t
    = fun includes gamma epsilon point gradient ->
    Printf.sprintf "descent :%s"
        (FloatVec.to_string point)
    |> print_endline;
    let point' = next_point gamma gradient point in
    if not (includes point')
    then point
    else if FloatVec.norm sqrt (FloatVec.sub point' point) < epsilon
        then point'
        else descent includes gamma epsilon point' gradient

(**
 * Conversion from an expression into a floating-point polynomial.
 *)
let rec expr_to_poly : Csp.expr -> P.Float.t
    = Csp.(function
    | Var v -> P.Float.of_var v
    | Cst (i, _) -> P.Float.of_constant (P.FloatRing.of_rational i)
    | Unary  (NEG, e) -> P.Float.neg (expr_to_poly e)
    | Binary (ADD, e1, e2) -> P.Float.add (expr_to_poly e1) (expr_to_poly e2)
    | Binary (SUB, e1, e2) -> P.Float.sub (expr_to_poly e1) (expr_to_poly e2)
    | Binary (MUL, e1, e2) -> P.Float.mul (expr_to_poly e1) (expr_to_poly e2)
    | Binary (DIV, e1, e2) -> begin
        match P.Float.div (expr_to_poly e1) (expr_to_poly e2) with
        | Some p -> p
        | _ -> Pervasives.invalid_arg "expr_to_poly:div"
        end
    | Binary (POW, e1, e2) -> begin
        match P.Float.pow (expr_to_poly e1) (expr_to_poly e2) with
        | Some p -> p
        | _ -> Pervasives.invalid_arg "expr_to_poly:div"
        end
    | _ -> Pervasives.invalid_arg "expr_to_poly")

(**
  * [gradient_descent x includes jacobian] returns the point resulting from the gradient descent along [jacobian], starting at [x].
  * @param includes returns true if the given point lies within a certain domain
  * @param x must belong to the domain w.r.t [includes]
  *)
let gradient_descent : FloatVec.t -> (FloatVec.t -> bool) -> Csp.ctrs -> FloatVec.t option
    = fun starting_point includes jacobian ->
    print_endline "gradient descent";
    List.iter
        (fun (_,jacob) -> List.iter
            (fun (var,expr) ->
                Printf.sprintf "%s -> %s\n"
                    var (expr_to_poly expr |> P.Float.to_string)
                |> print_endline)
            jacob)
        jacobian;
    let gamma = 0.1
    and epsilon = 0.01
    in
    (* Gradient of the input polynomial : *)
    (*let gradient = List.fold_left
        (fun map (bexpr,jacob) ->
            if Csp.is_cons_linear bexpr
            then map
            else List.fold_left
                (fun map (var,expr) ->
                    Tools.VarMap.add var (expr_to_poly expr) map
                )
                map jacob
        )
        Tools.VarMap.empty jacobian
    in
    *)
    let vars = List.hd jacobian
        |> Pervasives.snd
        |> List.map Pervasives.fst
    in
    Printf.sprintf "Variables: %s"
        (String.concat ", " vars)
        |> print_endline;
    try
        List.find (
            fun (bexpr, _) -> not (Csp.is_cons_linear bexpr)
            ) jacobian
        |> Pervasives.fst
        |> function
        | Csp.Cmp (op,e1,e2) ->
            let bexpr' = Csp.(Cmp (op,
                Binary (POW, e1, Cst (Mpqf.of_int 2, Csp.Int)),
                Binary (POW, e2, Cst (Mpqf.of_int 2, Csp.Int))))
            in
            let vars' = List.map (
                fun v -> (Csp.Real, v, Csp.Top))
                vars
            in
            let gradient = Csp.ctr_jacobian bexpr' vars'
                |> List.fold_left (
                    fun map (var,expr) ->
                    Tools.VarMap.add var (expr_to_poly expr) map
                    )
                    Tools.VarMap.empty
            in
            Printf.sprintf "Gradient : %s" (
                Tools.VarMap.bindings gradient
                |> List.map (
                    fun (var,poly) -> Printf.sprintf "%s -> %s"
                    var
                    (P.Float.to_string poly) )
                |> String.concat "\n"
            ) |> print_endline;
            let res = descent includes gamma epsilon starting_point gradient
            in
            Printf.sprintf "gradient result:%s" (FloatVec.to_string res)
                |> print_endline;
            Some res
        | _ -> None
    with Not_found -> None
