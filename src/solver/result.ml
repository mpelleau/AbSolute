(* Module that handles solution of the abstract solver *)
open Adcp_sig

type 'a res = {
    sure       : ('a * Csp.csts) list;   (* elements that satisfy the constraints *)
    unsure     : ('a * Csp.csts) list;   (* elements that MAY satisfy the constraints *)
    nb_sure    : int;       (* size of sure list *)
    nb_unsure  : int;       (* size of unsure list *)
    vol_sure   : float;     (* volume of the elements in the sure list *)
    vol_unsure : float;     (* volume of the elements in the unsure list *)
    nb_steps   : int;       (* number of steps of the solving process *)
    best_value : float      (* best value found during the optimization *)
  }

(* the abstract result type we'll be manipulating *)
module Make (A: AbstractCP) = struct

  type t = A.t res

  (* the empty result *)
  let empty_res = {
      sure       = [];
      unsure     = [];
      nb_sure    = 0;
      nb_unsure  = 0;
      vol_sure   = 0.;
      vol_unsure = 0.;
      nb_steps   = 0;
      best_value = 0.
    }

  (* adds an unsure element to a result *)
  let add_u res ?obj:fobj (u, c) =
    match !Constant.sure with
    | true -> res
    | false -> (
    match fobj with
    | Some fobj -> 
       let (obj_value, _) = A.forward_eval u fobj in
       if obj_value > res.best_value then
         res
       else if obj_value < res.best_value then
         {sure       = [];
          unsure     = [(u, c)];
          best_value = obj_value; 
          nb_unsure  = 0; 
          nb_sure    = 1; 
          nb_steps   = res.nb_steps;
          vol_unsure = A.volume u;
          vol_sure   = 0.}
       else
         {res with unsure     = (u, c)::res.unsure;
                   nb_unsure  = res.nb_unsure + 1;
                   vol_unsure = res.vol_unsure +. A.volume u}
    | None -> 
       {res with unsure     = (u, c)::res.unsure;
                 nb_unsure  = res.nb_unsure+1;
                 vol_unsure = res.vol_unsure+.A.volume u}
    )

  (* adds a sure element to a result *)
  let add_s res ?obj:fobj (s, c) =
    match fobj with
    | Some fobj -> 
       let (obj_value, _) = A.forward_eval s fobj in
       if obj_value > res.best_value then
         res
       else if obj_value < res.best_value then
         {sure       = [(s, c)];
          unsure     = []; 
          best_value = obj_value; 
          nb_sure    = 1; 
          nb_unsure  = 0; 
          nb_steps   = res.nb_steps;
          vol_sure   = A.volume s;
          vol_unsure = 0.}
       else
         {res with sure     = (s, c)::res.sure;
                   nb_sure  = res.nb_sure + 1;
                   vol_sure = res.vol_sure +. A.volume s}
    | None ->
       {res with sure     = (s, c)::res.sure;
                 nb_sure  = res.nb_sure+1;
                 vol_sure = res.vol_sure +. A.volume s}

  (* increments the step number of the solving process *)
  let incr_step res = {res with nb_steps = res.nb_steps+1}

  (* tests if a result can't be splitted anymore *)
  let stop res abs =
    res.nb_sure + res.nb_unsure > !Constant.max_sol
    || res.nb_steps > !Constant.max_iter

  (* tests if a result can't be pruned anymore *)
  let prunable res =
    !Constant.pruning && res.nb_steps < !Constant.pruning_iter

  (* creates an empty res for the optimization *)
  let empty_obj_res abs obj =
    let (_,obj_value) = A.forward_eval abs obj in
      {empty_res with best_value = obj_value}

  (* prints a result *)
  let print fmt res =
    Format.fprintf fmt "\n#inner boxes: %d\n#boundary boxes: %d\n#created nodes: %d\n\ninner volume = %f\nboundary volume = %f\ntotal volume = %f%!\n"
                   res.nb_sure
                   (if !Constant.sure then 0
                    else res.nb_unsure)
                   res.nb_steps
                   res.vol_sure
                   (if !Constant.sure then 0.
                    else res.vol_unsure)
                   (if !Constant.sure then res.vol_sure
                    else res.vol_sure +. res.vol_unsure)

end
