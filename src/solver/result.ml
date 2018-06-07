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
    best_value : Mpqf.t      (* best value found during the optimization *)
  }

(* the abstract result type we will be manipulating *)
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
      best_value = Csp.zero_val
    }



  let to_abs abs consts views =
    (*Format.printf "\n ---------- \n";
    List.iter (fun v -> Format.printf "%a " Csp.print_var v) (A.vars abs);*)
    let csts_expr = Csp.csts_to_expr consts in
    let (csts_vars, _) = List.split consts in
    (*Format.printf "\n ********** \n";
    List.iter (fun v -> Format.printf "%a " Csp.print_var v) csts_vars;*)

    let (views_vars, views_expr) = List.split views in
    (*Format.printf "\n vvvvvvvvvv \n";
    List.iter (fun v -> Format.printf "%a " Csp.print_var v) views_vars;*)

    let new_vars = List.map (fun v -> (Csp.REAL, v)) (csts_vars@views_vars) in
    let new_a = List.fold_left (A.add_var) abs new_vars in
    (*Format.printf "\n !!!!!!!!!! \n";
    List.iter (fun v -> Format.printf "%a " Csp.print_var v) (A.vars new_a);*)

    let new_a' = List.fold_left (fun a c -> A.filter a c) new_a csts_expr in

    let (vars, csts) = List.fold_left (
                            fun (a, c) (v, e) ->
                            let (l, u) as d = A.forward_eval new_a' e in
                            let id = List.hd (Csp.get_vars_expr e) in
                            if List.mem id csts_vars
                            then (a, (v, d)::c)
                            else ((v, d)::a, c)
                          ) ([], []) views in
    let to_add = Csp.csts_to_expr (vars@csts) in
    (*Format.printf "\n ////////// \n";
    List.iter (fun v -> Format.printf "%a " Csp.print_aux_csts v) vars;
    Format.printf "\n ?????????? \n";
    List.iter (fun v -> Format.printf "%a " Csp.print_aux_csts v) csts;

    Format.printf "\n\n %%%%%%%%%% \n";
    Format.printf "%a " A.print new_a';
    Format.printf "\n %%%%%%%%%% \n";*)


    (List.fold_left (fun a c -> A.filter a c) new_a' to_add, csts@consts)


  let get_solution (views:Csp.jacob) ?obj:fobj (abs:A.t) (consts:Csp.csts) =
    let (abs', csts) = to_abs abs consts views in
    let volume = A.volume abs' in
    let obj_value = match fobj with
      | Some fobj -> let (l, _) = A.forward_eval abs' fobj in l
      | None -> Csp.zero_val
    in

    (*Format.printf "\n $$$$$$$$$$ \n";
    List.iter (fun (v, d) -> Format.printf "%a " Csp.print_var v) csts;*)
    let abs' = List.fold_left (fun a (id, _) -> A.rem_var a id) abs' csts in
    (*Format.printf "\n ;;;;;;;;;; \n";
    List.iter (fun v -> Format.printf "%a " Csp.print_var v) (A.vars abs');
    Format.printf "\n";*)

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
    let vars = A.vars s in
    let const = List.map (fun v -> (v, A.var_bounds s v)) vars in
    let c = const@c in
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
         {res with sure     = (A.empty, c)::res.sure;
                   nb_sure  = res.nb_sure + 1;
                   vol_sure = res.vol_sure +. A.volume s}
    | None ->
       let (u, c, v, obj_value) = get_solution v s c in
       {res with sure     = (A.empty, c)::res.sure;
                 nb_sure  = res.nb_sure+1;
                 vol_sure = res.vol_sure +. v}

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
                   res.nb_unsure
                   res.nb_steps
                   res.vol_sure
                   res.vol_unsure
                   (res.vol_sure +. res.vol_unsure)

end
