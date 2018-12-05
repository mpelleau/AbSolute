let fail () = Pervasives.failwith "Boxed_octagon_drawer: unimplemented"

module Make(A:Adcp_sig.AbstractCP) = struct
  type t = A.t
  let is_empty = A.is_empty
  let print = A.print
  let bound = A.var_bounds

  let to_abs _ = fail ()

  let draw2d : t -> (Csp.var * Csp.var) -> Graphics.color -> unit
    = fun o (v1, v2) col -> ()

  let print_latex : Format.formatter -> t -> (Csp.var * Csp.var) -> Graphics.color -> unit
      = fun _ _ _ _ -> ()

  let draw3d : Format.formatter -> t list -> (Csp.var * Csp.var * Csp.var) -> unit
      = fun _ _ _ -> ()
end