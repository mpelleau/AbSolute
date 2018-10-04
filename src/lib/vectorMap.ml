module P = Polynom

(**
 * Type of vectors
 * Implemented as maps associating coefficients to variables.
 *)
module Make (Coeff : P.Ring) = struct

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
	let map : (Coeff.t -> 'a) -> ('a -> bool) -> t -> 'a M.t
		= fun f is_zero vec ->
		M.fold
			(fun var c map ->
				let c' = f c in
				if is_zero c'
				then M.remove var map
				else M.add var c' map)
			vec
            nil

    (**
     * Negation of each coefficient of the vector.
     *)
	let neg : t -> t
		= fun x ->
		map (fun c -> Coeff.neg c) (fun c -> Coeff.equal Coeff.zero c) x

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
        map (fun v' -> Coeff.mul n v') (fun c -> Coeff.equal Coeff.zero c) v


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
module FloatVec = Make(P.FloatRing)
module RationalVec = Make(P.RationalRing)
