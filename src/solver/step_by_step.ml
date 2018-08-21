module Make(Abs : Adcp_sig.AbstractCP) (Dr:Drawer_sig.Drawer with type t = Abs.t) = struct

  include Splitter.Make(Abs)
  module Print = Out.Make(Dr)

  let iof = int_of_float

  let color_sure = Graphics.rgb 100 200 255
  let color_unsure = Graphics.green
  let color_not_yet = Graphics.rgb 220 220 220
  let color_selected = Graphics.rgb 255 200 200


  let draw_s v1 v2 abs =
    Dr.draw2d abs (v1,v2) color_sure;
    View.draw_end v1 v2

  let draw_u v1 v2 abs =
    Dr.draw2d abs (v1,v2) color_unsure; View.draw_end v1 v2
  let draw_bad v1 v2 abs =
    Dr.draw2d abs (v1,v2) Graphics.red;
    View.draw_end v1 v2

  let draw_n v1 v2 abs =
    Dr.draw2d abs (v1,v2) color_not_yet;
    View.draw_end v1 v2

  let select v1 v2 abs =
    Dr.draw2d abs (v1,v2) color_selected;
    View.draw_end v1 v2

  let draw_wait v1 v2 abs =
    Dr.draw2d abs (v1,v2) Graphics.yellow;
    View.draw_end v1 v2

  let do_if_not_none f = function
    | Some a -> f a
    | None -> ()

  let explore (abs:Abs.t) (constrs:Csp.ctrs) (consts:Csp.csts) (view:Csp.jacob) v1 v2 =
    let selected = ref None in
    let draw_s = draw_s v1 v2 in
    let draw_u = draw_u v1 v2 in
    let draw_n = draw_n v1 v2 in
    let select = select v1 v2 in
    let draw_wait = draw_wait v1 v2 in
    (*let draw_bad = draw_bad v1 v2 in*)
    let switch abs =
      do_if_not_none draw_n !selected;
      selected := Some abs;
      select abs
    in
    let rec aux abs cstrs csts iter =
      switch abs;
      Unix.pause();
      match consistency abs cstrs csts with
      | Empty -> draw_n abs; selected := None;
      | Full (abs', const) -> draw_n abs; selected := None; draw_s abs'
      | Maybe(a,cstrs,_) when Abs.is_small a || iter > !Constant.max_iter ->
           draw_n abs; selected := None; draw_u a
      | Maybe(abs',cstrs,csts) ->
         switch abs';
         Unix.pause ();
         if !Constant.pruning then
           assert false
         else
           let splits = split abs' cstrs in
           List.iter (fun elem -> selected := None; draw_wait elem) splits;
           List.iter (fun elem ->
               aux elem cstrs csts (iter +1)
	           ) splits
    in aux abs constrs consts 0

  let solving prob =
    View.create_window 800 800;
    let _ = input_line stdin in
    let vars = Print.draw_vars prob in
    let size = Array.length vars in
    let(v1,v2) =(vars.(0)),(vars.(1 mod size)) in
    let open Csp in
    let (_,_,dom1) = List.find (fun (_,v',_) -> v1 = v') prob.init in
    let (_,_,dom2) = List.find (fun (_,v',_) -> v2 = v') prob.init in
    (match dom1,dom2 with
    | Finite(l1,h1),Finite(l2,h2)-> View.init (Mpqf.to_float l1,Mpqf.to_float h1) (Mpqf.to_float l2,Mpqf.to_float h2)
    | _ -> failwith "non finite domain");
    let abs = init prob in
    Format.printf "abs = %a\tvolume = %f@." Abs.print abs (Abs.volume abs);
    explore abs prob.Csp.jacobian prob.Csp.constants prob.Csp.view v1 v2
end
