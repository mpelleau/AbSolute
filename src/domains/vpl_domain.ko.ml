open Csp

(* indicates if the vpl is installed or not *)
let available = false

let fail () = failwith "VPLDomain: uninstalled"

module VplCP (* : Domain_signature.AbstractCP *)= struct

    type t = unit

    (* bornage d'une expression *)
    let forward_eval : t -> expr -> (Mpqf.t * Mpqf.t)
        = fun _ _ -> fail ()

    (* removes an unconstrained variable to the environnement *)
    let rem_var : t -> var -> t
        = fun _ _ -> fail ()

    let bounds : t -> csts
        = fun _ -> fail ()

    (* returns the bounds of a variable *)
    let var_bounds : t -> var -> (Mpqf.t * Mpqf.t)
        = fun _ _ -> fail ()

    let empty = ()

    let meet _ _ = fail ()

    let join _ _ = fail ()

    let is_empty _ = fail ()

    let vars _ = fail ()

    let add_var : t -> Csp.decl -> t
        = fun _ _ -> fail ()

    let volume : t -> float
        = fun _ -> fail ()

    let is_small : t -> bool
        = fun _-> fail ()

    let prune : (t -> t -> t list) option
      = Some (fun _ _ -> fail ())

    let split : t -> Csp.ctrs -> t list
        = fun _ -> fail ()

    (* TODO: can we use this variable? *)
    let split_along : t -> Csp.var -> t list
        = fun _ _ -> fail ()

    (* assume e1 cmp e2 *)
    let filter : t -> (Csp.expr * Csp.cmpop * Csp.expr) -> t Consistency.t
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

    let to_bexpr _ = fail ()

    let is_representable _ = fail ()

    let render _ = fail ()
end

let setup_flags : unit -> unit
    = fun () -> ()

let set_lin _ = ()

let enable_debug : unit -> unit
    = fun () -> ()

let start_profile () = ()
let stop_profile () = ()
let report () = ()
