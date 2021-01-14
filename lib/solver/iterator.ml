open Signature
open Consistency

(** this wrapper internalizes the constraints into an internal state and then
    handles the order of propagations *)
module Make (D : Domain) = struct
  type space = D.t

  type t = {space: space; constr: D.internal_constr list}

  let init ?(verbose = false) (p : Csp.problem) : t =
    if verbose then Format.printf "variable declaration ...%!" ;
    let space = List.fold_left D.add_var D.empty p.Csp.init in
    if verbose then Format.printf " done.\n" ;
    if verbose then Format.printf "constraint conversion ...%!" ;
    let constraints = List.map (D.internalize ~elem:space) p.Csp.constraints in
    if verbose then Format.printf " done.\n%!" ;
    {space; constr= constraints}

  (* filtering constraints in turn only once *)
  let propagate {space; constr} : t Consistency.t =
    let rec loop sat acc abs = function
      | [] -> Filtered ({space= abs; constr= acc}, sat)
      | c :: tl -> (
        match D.filter abs c with
        | Unsat -> Unsat
        | Sat -> loop sat acc abs tl
        | Filtered (abs', true) -> loop sat acc abs' tl
        | Filtered (abs', false) -> loop false (c :: acc) abs' tl )
    in
    loop true [] space constr

  let split f e = List.rev_map (fun space -> {e with space}) (D.split f e.space)

  let spawn elm = D.spawn elm.space

  let to_result ~inner res elm =
    if inner then Result.add_inner res elm.space
    else Result.add_outer res elm.space
end
