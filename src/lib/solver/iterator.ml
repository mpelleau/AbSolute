open Signature
open Consistency

(** this wrapper internalizes the constraints into an internal state and then
    handles the order of propagations *)
module Make (D : Domain) = struct
  type space = D.t

  type t = {space: space; constr: D.internal_constr list}

  let init (p : Csp.prog) : t =
    Format.printf "variable declaration ...%!" ;
    let space = List.fold_left D.add_var D.empty p.Csp.init in
    Format.printf " done.\n" ;
    Format.printf "constraint conversion ...%!" ;
    let constraints = List.map (D.internalize ~elem:space) p.Csp.constraints in
    Format.printf " done.\n%!" ;
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

  let split elm = List.map (fun e -> {elm with space= e}) (D.split elm.space)

  let spawn elm = D.spawn elm.space

  let to_result ~inner res elm =
    if inner then Result.add_inner res elm.space
    else Result.add_outer res elm.space
end
