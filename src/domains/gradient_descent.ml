module P = Polynom

(**
 * Type of vectors
 * Implemented as maps associating coefficients to variables.
 *)
module VectorMap (Coeff : P.Ring) = struct

    (**
     * Type of map
     *)
	module M = Tools.VarMap

    (**
     * Type of variables
     *)
	module Var = String

    (**
     * Type of vector
     *)
	type t = Coeff.t M.t

    (**
     * Empty vector: each variable is associated to coefficient 0.
     *)
    let nil = M.empty

    let to_string : t -> string
        = fun vec ->
        if vec = nil
        then "0"
        else
            Tools.VarMap.bindings vec
            |> List.map
                (fun (var,coeff) ->
                    Printf.sprintf "%s.%s"
                    var
                    (Coeff.to_string coeff)
                )
            |>
            String.concat " + "

    (**
     *  [set vec v n] sets the coefficient of variable [v] to [n] in vector [vec].
     *)
	let set : t -> Var.t -> Coeff.t -> t
        = fun vec var value ->
		if Coeff.equal value Coeff.zero
		then M.remove var vec
		else M.add var value vec

    (**
     * Returns the coefficient associated to the given variable in the vector.
     *)
    let get : t -> Var.t -> Coeff.t
		= fun vec var ->
        if M.mem var vec
		then M.find var vec
		else Coeff.zero

    (**
     * [mk z l] builds a map from the list of association [l].
	 *)
	let mk : (Coeff.t * Var.t) list -> t
        = fun i ->
		List.fold_left
            (fun v (n,var) -> set v var n)
            nil i

    (**
     * Applying a function to each element of the vector.
     *)
	let map : (Coeff.t -> Coeff.t) -> t -> t
		= fun f vec ->
		M.fold
			(fun var c map ->
				let c' = f c in
				if Coeff.equal Coeff.zero c'
				then M.remove var map
				else M.add var c' map)
			vec
            nil

    (**
     * Negation of each coefficient of the vector.
     *)
	let neg : t -> t
		= fun x ->
		map (fun c -> Coeff.neg c) x

    (**
     * Addition of two vectors.
     *)
	let add : t -> t -> t
		= fun v1 v2 ->
		M.merge
			(fun _ c1opt c2opt ->
				match c1opt,c2opt with
				| None, None -> None
				| None, Some c | Some c, None -> Some c
				| Some c1, Some c2 -> let c' = Coeff.add c1 c2 in
					if Coeff.equal Coeff.zero c'
					then None
					else Some c')
			v1 v2

    (**
     * Substraction of two vectors.
     *)
	let sub : t -> t -> t
		= fun v1 v2 ->
		add v1 (neg v2)

    (**
     * Multiplication of a vector and a coefficient.
     *)
	let mulc : Coeff.t -> t -> t
        = fun n v ->
        map (fun v' -> Coeff.mul n v') v


    let fold : (Var.t -> Coeff.t -> 'a -> 'a) -> t -> 'a -> 'a
        = M.fold

    (**
     * [norm sqrt vec] computes the L2 norm of the vector, using the given square root function [sqrt].
     *)
    let norm : (Coeff.t -> Coeff.t) -> t -> Coeff.t
        = fun sqrt vec ->
        fold
            (fun _ coeff acc -> Coeff.add coeff acc)
            vec Coeff.zero
        |> fun r -> sqrt r

end


(**
 * Type of floating points vectors.
 *)
module FloatVec = VectorMap(P.FloatRing)

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
let rec descent : float -> float -> FloatVec.t -> gradient -> FloatVec.t
    = fun gamma epsilon point gradient ->
    Printf.sprintf "descent :%s"
        (FloatVec.to_string point)
    |> print_endline;
    let point' = next_point gamma gradient point in
    if FloatVec.norm sqrt (FloatVec.sub point' point) < epsilon
    then point'
    else descent gamma epsilon point' gradient

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

let gradient_descent : Csp.ctrs -> FloatVec.t option
    = fun jacobian ->
    print_endline "gradient descent";
    List.iter
        (fun (_,jacob) -> List.iter
            (fun (var,expr) ->
                Printf.sprintf "%s -> %s\n"
                    var (expr_to_poly expr |> P.Float.to_string)
                |> print_endline)
            jacob)
        jacobian;
    let starting_point = FloatVec.nil
    and gamma = 0.01
    and epsilon = 0.00001
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
        let res = descent gamma epsilon starting_point gradient
        in
        Printf.sprintf "gradient result:%s" (FloatVec.to_string res)
            |> print_endline;
        Some res
    | _ -> None
