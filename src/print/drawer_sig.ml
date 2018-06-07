module type Drawer = sig

  type t

  val is_empty : t -> bool

  val to_abs : t * Csp.csts -> t
    
  val bound : t -> Csp.var -> Mpqf.t * Mpqf.t

  val draw2d : t -> (Csp.var * Csp.var) -> Graphics.color -> unit

  val print : Format.formatter -> t -> unit

  val print_latex :  Format.formatter -> t -> (Csp.var * Csp.var) -> Graphics.color -> unit

  val draw3d : Format.formatter -> t list -> (Csp.var * Csp.var * Csp.var) -> unit

end
