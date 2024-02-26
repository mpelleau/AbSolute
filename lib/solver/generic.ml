(** This module provides a functor that builds genereic and parametric
    resolution scheme from an abstract domain **)

open Signature
open Consistency

module Make (D : Domain) = struct
  module S = Iterator.Make (D)

  type ('a, 'b) acc = {inner: 'a list; outer: 'b list}

  let empty = {inner= []; outer= []}

  let is_empty {outer; _} = outer = []

  (* gets the element with highest priority *)
  let pop_outer c =
    match c.outer with
    | [] -> invalid_arg "no outer element"
    | h :: tl -> (h, {c with outer= tl})

  let add_inner c elm = {c with inner= elm :: c.inner}

  let add_outer cmp acc elm =
    let rec add_outer x = function
      | [] -> [x]
      | h :: tl -> if cmp x h > 0 then x :: h :: tl else h :: add_outer x tl
    in
    {acc with outer= add_outer elm acc.outer}

  let solve compare terminate prob =
    let csp = S.init prob in
    let compare e1 e2 = compare e1.S.space e2.S.space in
    let terminate e = terminate e.S.space in
    let rec aux acc =
      if is_empty acc then acc
      else
        let biggest, acc' = pop_outer acc in
        let new_acc =
          match S.propagate biggest with
          | Unsat -> acc'
          | Sat -> add_inner acc' biggest
          | Filtered (abs', true) -> add_inner acc' abs'
          | Filtered (abs', false) -> (
            try
              if terminate abs' then raise Too_small ;
              S.split abs' |> List.fold_left (add_outer compare) acc'
            with Too_small -> add_outer compare acc' abs' )
          | _ -> failwith "pruned"
        in
        aux new_acc
    in
    aux (add_outer compare empty csp)
end
