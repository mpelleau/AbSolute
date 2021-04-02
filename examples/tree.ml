open Libabsolute
open Consistency
module D = Domains.Utree.Make (Domains.BoxF)

let init (p : Csp.t) =
  let space = List.fold_left D.add_var D.empty p.Csp.variables in
  let constraints = List.map (D.internalize ~elem:space) p.Csp.constraints in
  (space, constraints)

let print_intern fmt c =
  Format.fprintf fmt "%a" Constraint.print (D.externalize c)

let pp_constr fmt = List.iter (print_intern fmt)

let () =
  let open Csp in
  let prob =
    empty
    |> add_real_var_f "x" (-1000.) 1000.
    |> add_real_var_f "t" 0. 1000.
    |> add_constr
         (Parser.constr
            {|(x in [0;10] && t in [0;1]) || (x in [5;15] && t in [1;2])|})
  in
  let s, c = init prob in
  Format.printf "%a, %a\n" D.print s pp_constr c ;
  Format.printf "Filtering the constraint:\n" ;
  let rec aux = function
    | [] -> Format.printf "all constraint processed\n%!"
    | h :: t ->
        ( match D.filter s h with
        | Sat -> Format.printf "%a satisfied\n" print_intern h
        | Unsat -> Format.printf "%a unsatisfiable\n" print_intern h
        | Filtered ((s', c'), _) ->
            Format.printf "new abstract value:\n %a\n" D.print s' ;
            Format.printf "new constraint:\n%a\n" print_intern c' ) ;
        aux t
  in
  aux c
