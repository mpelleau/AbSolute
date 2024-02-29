(** this example defines the csp for convex polygons of size n and output a
    graphviz file.

    It is possible to view it by doing:

    n=15; dune exec ./polygons.exe $n; neato -Goverlap=false -Tpng convex$n.viz
    > convex$n.png; open convex$n.png

    NB: neato produces a friendlier output than dot *)

open Libabsolute

(* vector from (x1,y1) yo (x2,y2) *)
let vector (x1, y1) (x2, y2) =
  let open Expr.Operators in
  (x2 - x1, y2 - y1)

(* cross product of two vectors *)
let cross_product (x1, y1) (x2, y2) =
  let open Expr.Operators in
  (x1 * y2) - (y1 * x2)

(* Function that builds the list of constraints corresponding to the check that
   a list of points forms a convex polygon. This is done by verifying that the
   vectors corresponding to two consecutive edges of the polygons have a
   negative cross-product (We could have also picked positive, but we enforce
   less representations for the same polyhedron that way). *)
let is_convex points =
  let rec check_convexity = function
    | p1 :: p2 :: p3 :: rest ->
        let v1 = vector p1 p2 in
        let v2 = vector p2 p3 in
        let open Constraint.Operators in
        (cross_product v1 v2 < Expr.zero) :: check_convexity (p2 :: p3 :: rest)
    | _ -> []
  in
  match points with
  | [] | [_] | [_; _] ->
      failwith "Invalid input: At least 3 points are required."
  | h1 :: h2 :: _ -> check_convexity (points @ [h1; h2])

(* build two variables xn, yn per points of the polygon *)
let build_vars n =
  let rec loop acc = function
    | 0 -> List.rev acc
    | n ->
        let idx = string_of_int n in
        loop (("x" ^ idx, "y" ^ idx) :: acc) (n - 1)
  in
  loop [] n

let build_csp n =
  let names = build_vars n in
  let csp =
    List.fold_left
      (fun csp (v1, v2) ->
        csp |> Csp.add_real_var_f v1 0. 100. |> Csp.add_real_var_f v2 0. 100. )
      Csp.empty names
  in
  let var_exprs = List.map (fun (v1, v2) -> (Expr.var v1, Expr.var v2)) names in
  let constrs = is_convex var_exprs in
  List.fold_left Csp.add_constr csp constrs

let () =
  if Array.length Sys.argv < 2 then (
    Format.printf "bad usage: expect a number of points\n" ;
    exit 1 ) ;
  match int_of_string_opt Sys.argv.(1) with
  | Some nb_points ->
      let csp = build_csp nb_points in
      Format.printf "%a\n" Csp.print csp ;
      Csp.to_graphviz csp (Format.sprintf "convex%i.viz" nb_points)
  | None ->
      Format.printf "expecting numeric argument %s\n" Sys.argv.(1) ;
      exit 1
