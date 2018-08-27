
module Product_Drawer (R : Product.Reduction) (D : Drawer_sig.Drawer with type t = R.B.t) =
  struct
    type t = R.t

    let bound = R.var_bounds

    let to_abs (abs, consts) =
      let csts_expr = Csp.csts_to_expr consts in
      let (csts_vars, _) = List.split consts in

      let new_vars = List.map (fun v -> (Csp.Real, v)) (csts_vars) in
      let a = List.fold_left (R.add_var) abs new_vars in

      List.fold_left (fun a c -> R.filter a c) a csts_expr

    let draw2d (a, b) (v1, v2) col =
      let (a', b') = R.reduced_product a b in
      D.draw2d b' (v1, v2) col


    let draw3d fmt abs_list (v1,v2,v3) =
      let l = List.map (fun (a, b) -> let (a', b') = R.reduced_product a b in b') abs_list in
      D.draw3d fmt l (v1, v2, v3)

    let print_latex fmt (a, b) (v1,v2) col =
      let (a', b') = R.reduced_product a b in
      D.print_latex fmt b' (v1, v2) col

    let print = R.print

    let is_empty = R.is_empty
  end

module BoxNoctDrawer = Product_Drawer(Product.BoxAndOct)(Apron_drawer.OctDrawer)
module BoxNpolyDrawer = Product_Drawer(Product.BoxAndPoly)(Apron_drawer.PolyDrawer)
module OctNpolyDrawer = Product_Drawer(Product.OctAndPoly)(Apron_drawer.PolyDrawer)
module BandPDrawer = Product_Drawer(Product.BoxAndPolyNew)(Apron_drawer.PolyDrawer)
