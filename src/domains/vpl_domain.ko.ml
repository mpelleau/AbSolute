let fail () = Pervasives.failwith "VPLDomain: uninstalled"

module VplCP (* : Domain_signature.AbstractCP *)= struct

    type t = unit

    let empty = ()

    let meet _ _ = fail ()

    let join _ _ = fail ()

    let add_var : t -> Csp.typ * Csp.var * Csp.dom -> t
        = fun _ _ -> fail ()

    let volume : t -> float
        = fun _ -> fail ()

    let is_small : t -> bool
        = fun _-> fail ()

    (* Note: the last t is the intersection between the two operands *)
    let prune : t -> t -> t list * t
        = fun _ _ -> fail ()

    let split : t -> t list
        = fun _ -> fail ()

    (* TODO: can we use this variable? *)
    let split_along : t -> Csp.var -> t list
        = fun _ _ -> fail ()

    let filter : t -> (Csp.expr * Csp.cmpop * Csp.expr) -> t
        = fun _ _ -> fail ()

    (* TODO: Should return the variable with the maximal range as well. *)
    let filter_maxvar : t -> (Csp.expr * Csp.cmpop * Csp.expr) -> t * (Csp.var*float)
        = fun _ _ -> fail ()

    (* TODO: use Format *)
    let print : Format.formatter -> t -> unit
        = fun _ _ -> fail ()

    (* TODO: to define *)
    let spawn : t -> Csp.instance
        = fun _ -> fail ()

    (* TODO: to define *)
    let is_abstraction : t -> Csp.instance -> bool
        = fun _ -> fail ()

end

let setup_flags : unit -> unit
    = fun () -> ()

let set_lin _ = ()

let enable_debug : unit -> unit
    = fun () -> ()

let start_profile () = ()
let stop_profile () = ()
let report () = ()
