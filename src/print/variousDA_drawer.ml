
module Reduction_Drawer(AP : VariousDA.Reduction) (D : Drawer_sig.Drawer with type t = AP.B.t) = struct
 
  module T = VariousDA.VariousDomain_MS(AP)
  type t = AP.A.t * AP.B.t

  let bound = T.var_bounds    

  let to_abs (abs, consts) =
    let csts_expr = Csp.csts_to_expr consts in
    let (csts_vars, _) = List.split consts in

    let new_vars = List.map (fun v -> (Csp.REAL, v)) (csts_vars) in
    let a = List.fold_left (T.add_var) abs new_vars in

    List.fold_left (fun a c -> T.filter a c) a csts_expr

  let draw2d (a, b) (v1, v2) col =
    let (a', b') = T.reduced_product a b in
    D.draw2d b' (v1, v2) col
    

  let draw3d fmt abs_list (v1,v2,v3) =
    let l = List.map (fun (a, b) -> let (a', b') = T.reduced_product a b in b') abs_list in
    D.draw3d fmt l (v1, v2, v3)

  let print_latex fmt (a, b) (v1,v2) col =
    let (a', b') = T.reduced_product a b in
    D.print_latex fmt b' (v1, v2) col

  let print = T.print

  let is_empty = T.is_empty
  
end

module BoxNoctDrawer = Reduction_Drawer(VariousDA.BoxAndOct)(Apron_drawer.OctDrawer)
module BoxNpolyDrawer = Reduction_Drawer(VariousDA.BoxAndPoly)(Apron_drawer.PolyDrawer)
module OctNpolyDrawer = Reduction_Drawer(VariousDA.OctAndPoly)(Apron_drawer.PolyDrawer)
module BandPDrawer = Reduction_Drawer(VariousDA.BoxAndPolyNew)(Apron_drawer.PolyDrawer)
