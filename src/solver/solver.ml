open Signature
open Consistency

(** This module builds different resolution scheme according to a
   solvable module **)
module Solve(Abs : AbstractCP) = struct

  include Splitter.Make(Abs)
  include Result.Make(Abs)

  (* prints (in-place) the current approximated progression of the
     solving: when a searchspace is processed (Sat, unsat or too
     small) at a given depth d, we increase the progress bar by
     1/(2^d). *)
  let loading =
    let progress = ref Q.zero in
    let print = Tools.inplace_print () in
    fun depth ->
    let p = Mpz.init() in
    Mpz.mul_2exp p (Mpz.of_int 1) (depth-1);
    progress := Mpqf.add !progress  (Mpqf.inv (Mpqf.of_mpz p));
    if Mpqf.cmp !progress Q.one >= 0 then
      Format.printf "%a\n%!" print "done."
    else
      Mpqf.mul !progress (Mpqf.of_int 100)
      |> Mpqf.to_float |> Float.to_int
      |> Format.asprintf "%i%%"
      |> Format.printf "%a%!" print

  (* increases the loading bar by d and returns the value v *)
  let return d v = loading d; v

  let explore (abs:Abs.t) constrs =
    (* propagation/exploration loop *)
    let rec aux cstrs depth res abs =
      match consistency abs cstrs with
      | Unsat -> res
      | Sat -> add_s res abs
      | Filtered(a, _) when stop res a || Abs.is_small a -> add_u res a
      | Filtered(abs', _) ->
         List.fold_left (fun res elem ->
             aux cstrs (depth +1) (incr_step res) elem
           ) res (split abs' [])
    in
    aux constrs 0 empty_res abs

  (* entry point of the solver *)
  let solving prob =
    Tools.debug 1 "entering the resolution\n";
    let abs = init prob in
    let res = explore abs prob.constraints in
    res
end
