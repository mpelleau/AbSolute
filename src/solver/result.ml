(* Module that handles solution of the abstract solver *)

type 'a res = {
    sure       : ('a * Csp.csts) list;   (* elements that satisfy the constraints *)
    unsure     : ('a * Csp.csts) list;   (* elements that MAY satisfy the constraints *)
    nb_sure    : int;       (* size of sure list *)
    nb_unsure  : int;       (* size of unsure list *)
    vol_sure   : float;     (* volume of the elements in the sure list *)
    vol_unsure : float;     (* volume of the elements in the unsure list *)
    nb_steps   : int;       (* number of steps of the solving process *)
    best_value : Mpqf.t     (* best value found during the optimization *)
  }

(* returns the inner ratio (between 0 and 1) of a solution *)
let inner_ratio res =
  let divider = res.vol_sure +. res.vol_unsure in
  if divider = 0. then 0.
  else res.vol_sure /. divider


(* Shapes that have a volume, and can be evaluated *)
module type Res = sig
  type t

  val empty : t

  val forward_eval : t -> Csp.expr -> Mpqf.t * Mpqf.t

  val add_var : t -> Csp.annot * Csp.var -> t

  val filter : t -> (Csp.expr * Csp.cmpop * Csp.expr) -> t

  val volume : t -> float
end

(* the abstract result type we will be manipulating *)
module Make (A: Res) = struct

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
      best_value = Q.zero
    }

  let to_abs abs consts views =
    let csts_expr = Csp_helper.csts_to_expr consts in
    let (csts_vars, _) = List.split consts in
    let (views_vars, views_expr) = List.split views in
    let new_vars = List.map (fun v -> (Csp.Real, v)) (csts_vars@views_vars) in
    let new_a = List.fold_left (A.add_var) abs new_vars in
    let new_a' = List.fold_left (fun a c -> A.filter a c) new_a csts_expr in
    let (vars, csts) = List.fold_left (
                            fun (a, c) (v, e) ->
                            let (l, u) as d = A.forward_eval new_a' e in
                            let id = List.hd (Csp_helper.get_vars_expr e) in
                            if List.mem id csts_vars
                            then (a, (v, d)::c)
                            else ((v, d)::a, c)
                          ) ([], []) views in
    let to_add = Csp_helper.csts_to_expr (vars@csts) in
    (List.fold_left (fun a c -> A.filter a c) new_a' to_add, csts@consts)


  let get_solution (views:Csp.jacob) ?obj:fobj (abs:A.t) (consts:Csp.csts) =
    let (abs', csts) = to_abs abs consts views in
    let volume = A.volume abs' in
    let obj_value = match fobj with
      | Some fobj -> let (l, _) = A.forward_eval abs' fobj in l
      | None -> Q.zero
    in
    (abs', csts, volume, obj_value)


  (* adds an unsure element to a result *)
  let add_u res ?obj:fobj (u, c, v) =
    match !Constant.sure with
    | true -> res
    | false -> (
    match fobj with
    | Some fobj ->
       let (u, c, v, obj_value) = get_solution v ~obj:fobj u c in
       if obj_value > res.best_value then
         res
       else if obj_value < res.best_value then
         {sure       = [];
          unsure     = [(u, c)];
          best_value = obj_value;
          nb_unsure  = 0;
          nb_sure    = 1;
          nb_steps   = res.nb_steps;
          vol_unsure = v;
          vol_sure   = 0.}
       else
         {res with unsure     = (u, c)::res.unsure;
                   nb_unsure  = res.nb_unsure + 1;
                   vol_unsure = res.vol_unsure +. v}
    | None ->
       let (u, c, v, obj_value) = get_solution v u c in
       {res with unsure     = (u, c)::res.unsure;
                 nb_unsure  = res.nb_unsure+1;
                 vol_unsure = res.vol_unsure+.v}
    )

  (* adds a sure element to a result *)
  let add_s res ?obj:fobj (s, c, v) =
    match fobj with
    | Some fobj ->
       let (s, c, v, obj_value) = get_solution v ~obj:fobj s c in
       if obj_value > res.best_value then
         res
       else if obj_value < res.best_value then
         {sure       = [(A.empty, c)];
          unsure     = [];
          best_value = obj_value;
          nb_sure    = 1;
          nb_unsure  = 0;
          nb_steps   = res.nb_steps;
          vol_sure   = v;
          vol_unsure = 0.}
       else
         {res with sure     = (s, c)::res.sure;
                   nb_sure  = res.nb_sure + 1;
                   vol_sure = res.vol_sure +. A.volume s}
    | None ->
       let (u, c, v, obj_value) = get_solution v s c in
       {res with sure     = (s, c)::res.sure;
                 nb_sure  = res.nb_sure+1;
                 vol_sure = res.vol_sure +. v}

  (* increments the step number of the solving process *)
  let incr_step res = {res with nb_steps = res.nb_steps+1}

  (* tests if a result can't be splitted anymore *)
  let stop res abs =
    res.nb_sure + res.nb_unsure > !Constant.max_sol
    || res.nb_steps > !Constant.max_iter

  (* tests if a result can't be pruned anymore *)
  (* TO CHECK: In solver.ml, `Constant.pruning_iter` seems to be the maximal depth
               of the search tree under which we stop "pruning by finding inconsistencies",
               not the number of nodes explored (nb_steps = nb_nodes?).
  *)
  let prunable res =
    !Constant.pruning && res.nb_steps < !Constant.pruning_iter

  (* creates an empty res for the optimization *)
  let empty_obj_res abs obj =
    let (_,obj_value) = A.forward_eval abs obj in
    {empty_res with best_value = obj_value}

  (* iterates over the list of sure elements *)
  let iter_sure f res = List.iter f res.sure

  (* iterates over the list of unsure elements *)
  let iter_unsure f res = List.iter f res.unsure

  (* prints a result *)
  let print fmt res =
    Format.fprintf fmt "\n#inner boxes: %d\n#boundary boxes: %d\n#created nodes: %d\n\ninner volume = %f\nboundary volume = %f\ntotal volume = %f%!\n"
      res.nb_sure
      res.nb_unsure
      res.nb_steps
      res.vol_sure
      res.vol_unsure
      (res.vol_sure +. res.vol_unsure)
end
