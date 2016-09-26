(* Module that handles solution of the abstract solver *)

(* the abstract result type we'll be manipulating *)
type 'a t = {
  sure      : 'a list;   (* elements that satisfy the constraints *)
  unsure    : 'a list;   (* elements that MAY satisfy the constraints *)
  nb_sure   : int;       (* size of sure list *)
  nb_unsure : int;       (* size of unsure list *)
  nb_steps  : int        (* number of steps of the solving process *)
}

(* the empty result *)
let empty_res = {
  sure      = [];
  unsure    = [];
  nb_sure   = 0;
  nb_unsure = 0;
  nb_steps  = 0
}

  (* adds an unsure element to a result *)
let add_u res u =
  {res with unsure=(u::res.unsure); nb_unsure=res.nb_unsure+1}

  (* adds a sure element to a result *)
let add_s res s =
  {res with sure=(s::res.sure); nb_sure=res.nb_sure+1}

  (* tests if a result can't be splitted anymore *)
let stop res abs =
  res.nb_sure + res.nb_unsure > !Constant.max_sol
  || res.nb_steps > !Constant.max_iter

(* prints a result *)
let print fmt res =
  Format.fprintf fmt "#inner boxes: %d\n#boundary boxes: %d\n#created nodes: %d%!\n"
    res.nb_sure
    (if !Constant.sure then 0
     else res.nb_unsure)
    res.nb_steps
