(** This module provides a functor that builds different resolution scheme from
    an abstract domain **)

open Signature
open Consistency

module Make (D : Domain) (P : Propagator) = struct
  module P = P.Make (D)

  (* prints (in-place) the current approximated progression of the solving: when
     a searchspace is processed (Sat, Unsat or too small) at a given depth d, we
     increase the progress bar by 1/(2^d). *)
  let loading =
    let z1 = Mpz.of_int 1 in
    let progress = ref Q.zero in
    let print = Tools.inplace_print () in
    let q100 = Q.of_int 100 in
    let p = Mpz.init () in
    fun depth ->
      Mpz.mul_2exp p z1 (depth - 1) ;
      progress := Mpqf.add !progress (Mpqf.inv (Mpqf.of_mpz p)) ;
      if Mpqf.cmp !progress Q.one >= 0 then Format.printf "%a\n%!" print "done."
      else
        Q.mul !progress q100 |> Q.to_float |> Format.asprintf "%.2f%%"
        |> Format.printf "%a%!" print

  (* increases the loading bar by d and returns the value v *)
  let return ?(verbose = false) d v =
    if verbose then loading d ;
    v

  let coverage ?(verbose = false) prec max_depth prob : P.space Result.t =
    let csp = P.init ~verbose prob in
    if verbose then Format.printf "coverage ... %!" ;
    let rec aux depth res abs =
      match P.propagate abs with
      | Unsat -> return ~verbose depth res
      | Sat -> return depth (P.to_result ~inner:true res abs)
      | Filtered (abs', true) ->
          return ~verbose depth (P.to_result ~inner:true res abs')
      | Filtered (abs', false) -> (
          if depth >= max_depth then
            return ~verbose depth (P.to_result ~inner:false res abs')
          else
            try P.split ~prec abs' |> List.fold_left (aux (depth + 1)) res
            with Too_small ->
              return ~verbose depth (P.to_result ~inner:false res abs') )
      | Pruned {sure; unsure} ->
          let res' = List.fold_left (P.to_result ~inner:true) res sure in
          List.fold_left (aux (depth + 1)) res' unsure
      (* this (potentially) results in progress bar going over 100% ... but who
         cares? *)
    in
    aux 1 Result.empty csp

  let satisfiability ?(verbose = false) prec max_depth prob : Kleene.t =
    let csp = P.init ~verbose prob in
    let loading = if verbose then loading else ignore in
    if verbose then Format.printf "satisfiability ... %!" ;
    let rec aux depth abs =
      match P.propagate abs with
      | Sat -> loading 1 ; raise Exit
      | Filtered (_, true) -> loading 1 ; raise Exit
      | Unsat -> Kleene.False
      | Filtered (a, false) -> (
          if depth >= max_depth then return ~verbose depth Kleene.Unknown
          else
            try
              P.split ~prec a
              |> List.fold_left
                   (fun acc elem -> Kleene.or_ acc (aux (depth + 1) elem))
                   Kleene.False
            with Too_small -> return ~verbose depth Kleene.Unknown )
      | Pruned _ -> loading 1 ; raise Exit
    in
    try aux 1 csp with Exit -> Kleene.True

  exception Found of Csp.instance

  let found a = raise (Found (P.spawn a))

  let witness ?(verbose = false) prec max_depth prob : feasible =
    let csp = P.init ~verbose prob in
    let loading = if verbose then loading else ignore in
    if verbose then Format.printf "witness ... %!" ;
    let rec aux depth abs =
      match P.propagate abs with
      | Sat -> loading 1 ; found abs
      | Filtered (a, true) -> loading 1 ; found a
      | Filtered (a, false) -> (
          if depth >= max_depth then return depth Kleene.Unknown
          else
            try
              P.split ~prec a
              |> List.fold_left
                   (fun acc elem -> Kleene.or_ acc (aux (depth + 1) elem))
                   Kleene.False
            with Too_small -> return ~verbose depth Kleene.Unknown )
      | Unsat -> loading depth ; Kleene.False
      | Pruned {sure; _} -> found (List.hd sure)
    in
    let of_kleene = function
      | Kleene.Unknown -> Maybe
      | Kleene.False -> Unfeasible
      | Kleene.True -> assert false
    in
    try aux 1 csp |> of_kleene with Found i -> Witness i
end

module D = Domains.Boolean.Make (Domains.BoxS)
module Default = Make (D) (Iterator)
