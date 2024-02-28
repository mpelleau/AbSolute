(** this example defines the csp for sorted list of size n and output a graphviz
    file.

    It is possible to view it by doing:

    n=15; dune exec ./sortedlist.exe $n; neato -Goverlap=false -Tpng
    sortedlist$n.viz > sortedlist$n.png; open sortedlist$n.png

    NB: neato produces a friendlier output than dot *)

open Libabsolute

(* Function that builds the list of constraints corresponding to the check that
   a list of points forms a convex polygon. This is done by verifying that the
   vectors corresponding to two consecutive edges of the polygons have a
   negative cross-product (We could have also picked positive, but we enforce
   less representations for the same polyhedron that way). *)
let sorted l =
  let rec loop = function
    | x1 :: x2 :: rest ->
        let open Constraint.Operators in
        (x1 <= x2) :: loop (x2 :: rest)
    | _ -> []
  in
  match l with
  | [] | [_] -> failwith "Invalid input: At least 2 numbers are required."
  | _ -> loop l

(* build two variables xn, yn per points of the polygon *)
let build_vars n =
  let rec loop acc = function
    | 0 -> List.rev acc
    | n ->
        let idx = string_of_int n in
        loop (("x" ^ idx) :: acc) (n - 1)
  in
  loop [] n

let build_csp n =
  let names = build_vars n in
  let csp =
    List.fold_left
      (fun csp v -> csp |> Csp.add_real_var_f v 0. 1000.)
      Csp.empty names
  in
  let var_exprs = List.map Expr.var names in
  let constrs = sorted var_exprs in
  List.fold_left Csp.add_constr csp constrs

let () =
  if Array.length Sys.argv < 2 then (
    Format.printf "bad usage: expect a number of points\n" ;
    exit 1 ) ;
  match int_of_string_opt Sys.argv.(1) with
  | Some nb_points ->
      let csp = build_csp nb_points in
      Format.printf "%a\n" Csp.print csp ;
      Csp.to_graphviz csp (Format.sprintf "sortedlist%i.viz" nb_points)
  | None ->
      Format.printf "expecting numeric argument %s\n" Sys.argv.(1) ;
      exit 1
