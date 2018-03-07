open Adcp_sig
open ADCP

(* Solver *)
module Solve(Abs : AbstractCP) = struct

  include Splitter.Make(Abs)
  module Res = Result.Make(Abs)

  let prune abs cstrs =
    let open Constant in
    let comp_abs = get_complementary abs cstrs in
    let v_comp =
      List.fold_left (fun acc e ->
          match e with
          | None -> acc
          | Some e -> Abs.volume e +. acc) 0. comp_abs
    in
    if v_comp < !worthiness *. Abs.volume abs then
      match comp_abs with
      | [one] -> prune abs cstrs
      | comp_abs ->
         if !pj then prune_join abs comp_abs
         else prune abs cstrs
    else [],[abs]

  let split_prune abs cstrs =
    let comp_abs = get_complementary abs cstrs in
    match List.fold_left meetopt (Some abs) comp_abs with
    | None -> Abs.split abs
    | Some x ->
       let ls,lu = Abs.prune abs x in
       List.rev_map Abs.split (lu::ls) |> List.flatten

  let explore_alternate (abs:Abs.t) (constrs:Csp.constrs) =
    let open Res in
    let rec aux_p abs cstrs res depth =
       let ls,lu = prune abs cstrs in
       let res = List.fold_left (fun r x -> add_s r x) res ls in
       List.fold_left (fun res x ->
           List.fold_left (fun res x ->
               aux x cstrs (incr_step res) (depth +1)
             ) res (split x cstrs)
	       ) res lu
    and aux abs cstrs res depth =
       match consistency abs cstrs with
      | Empty -> res
      | Full abs' -> add_s res abs'
      | Maybe(a,cstrs) when stop res a || Abs.is_small a -> add_u res a
      | Maybe(abs',cstrs) ->
         List.fold_left (fun res elem ->
             aux_p elem cstrs (incr_step res) (depth +1)
	         ) res (split abs' cstrs)
    in aux_p abs constrs empty_res 0

  let explore_check_volume abs cstrs =
    let open Res in
    let rec choice abs cstrs res =
      let comp_abs = get_complementary abs cstrs in
      let v_comp =
        List.fold_left (fun acc e ->
            match e with
            | None -> acc
            | Some e -> Abs.volume e +. acc) 0. comp_abs
      in
      let ratio = v_comp /. Abs.volume abs in
      if ratio < 0.5 then elimination abs cstrs res
      else if ratio > 0.9 then splitting abs cstrs res 4
      else if ratio > 0.7 then splitting abs cstrs res 2
      else splitting abs cstrs res 1

    and propagation abs cstrs res : Res.t =
      match consistency abs cstrs with
      | Empty -> res
      | Full abs' -> add_s res abs'
      | Maybe(a,cstrs) when stop res a || Abs.is_small a -> add_u res a
      | Maybe(abs',cstrs) -> choice abs' cstrs (incr_step res)
         (* List.fold_left (fun res elem -> *)
         (*     choice elem cstrs (incr_step res) *)
	       (*   ) res (split abs' cstrs) *)

    and elimination abs cstrs res : Res.t =
      let ls,lu = prune abs cstrs in
      let res = List.fold_left (fun r x -> add_s r x) res ls in
      List.fold_left (fun res x -> choice x cstrs res) res lu

    and splitting abs cstrs res i : Res.t =
      if i = 0 then propagation abs cstrs res
      else
        let s = split abs cstrs in
        List.fold_left (fun res x -> splitting x cstrs (incr_step res) (i-1)) res s
    in
    propagation abs cstrs empty_res

end
