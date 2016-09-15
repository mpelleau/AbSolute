module type Drawer = sig

  type t

  val bound : t -> Csp.var -> float * float

  val print : Format.formatter -> t -> unit

  val draw2d : t -> (Csp.var * Csp.var) -> Graphics.color -> unit

  val draw3d : t list -> (Csp.var * Csp.var * Csp.var) -> unit

end
