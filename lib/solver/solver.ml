open Signature
open Consistency

(** This module builds different resolution scheme according to a solvable
    module **)
module Make (D : Domain) = struct
  module S = Iterator.Make (D)

  (* prints (in-place) the current approximated progression of the solving: when
     a searchspace is processed (Sat, Unsat or too small) at a given depth d, we
     increase the progress bar by 1/(2^d). *)
  let loading =
    let q100 = Q.of_int 100 in
    let z1 = Mpz.of_int 1 in
    let progress = ref Q.zero in
    let print = Tools.inplace_print () in
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

  (** coverage of the solution space*)
  let coverage ?(verbose = false) prec max_depth prob : S.space Result.t =
    let csp = S.init ~verbose prob in
    if verbose then Format.printf "coverage ... %!" ;
    let rec aux depth res abs =
      match S.propagate abs with
      | Unsat -> return ~verbose depth res
      | Sat -> return depth (S.to_result ~inner:true res abs)
      | Filtered (abs', true) ->
          return ~verbose depth (S.to_result ~inner:true res abs')
      | Filtered (abs', false) -> (
          if depth >= max_depth then
            return ~verbose depth (S.to_result ~inner:false res abs')
          else
            try S.split prec abs' |> List.fold_left (aux (depth + 1)) res
            with TooSmall ->
              return ~verbose depth (S.to_result ~inner:false res abs') )
    in
    aux 1 Result.empty csp

  (** satisfiability check *)
  let satisfiability ?(verbose = false) prec max_depth prob : Kleene.t =
    let csp = S.init ~verbose prob in
    if verbose then Format.printf "satisfiability ... %!" ;
    let rec aux depth abs =
      match S.propagate abs with
      | Sat ->
          if verbose then loading 1 ;
          raise Exit
      | Filtered (_, true) ->
          if verbose then loading 1 ;
          raise Exit
      | Unsat -> Kleene.False
      | Filtered (a, false) -> (
          if depth >= max_depth then return ~verbose depth Kleene.Unknown
          else
            try
              S.split prec a
              |> List.fold_left
                   (fun acc elem -> Kleene.or_kleene acc (aux (depth + 1) elem))
                   Kleene.False
            with TooSmall -> return ~verbose depth Kleene.Unknown )
    in
    try aux 1 csp with Exit -> Kleene.True

  (** satisfiability check, with witness *)
  let witness ?(verbose = false) prec max_depth prob :
      Kleene.t * Csp.instance option =
    let csp = S.init ~verbose prob in
    if verbose then Format.printf "witness ... %!" ;
    let exception Found of Csp.instance in
    let rec aux depth abs =
      match S.propagate abs with
      | Sat ->
          if verbose then loading 1 ;
          raise (Found (S.spawn abs))
      | Filtered (a, true) ->
          if verbose then loading 1 ;
          raise (Found (S.spawn a))
      | Filtered (a, false) -> (
          if depth >= max_depth then return depth Kleene.Unknown
          else
            try
              S.split prec a
              |> List.fold_left
                   (fun acc elem -> Kleene.or_kleene acc (aux (depth + 1) elem))
                   Kleene.False
            with TooSmall -> return ~verbose depth Kleene.Unknown )
      | Unsat ->
          if verbose then loading depth ;
          Kleene.False
    in
    try (aux 1 csp, None) with Found i -> (Kleene.True, Some i)
end
