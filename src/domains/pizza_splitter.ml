module VPL = Vpl_domain.VplCP
open Gradient_descent

module Make (D : sig
    include Adcp_sig.AbstractCP
    val to_vpl: t -> VPL.t
    val of_vpl: VPL.t -> t
    val to_ident: Csp.var -> Vpl.Var.Positive.t
    end) =
struct

    include D

    let split : t -> Csp.ctrs -> t list
        = fun p jacobian ->
        match !Constant.split with
            | "pizza" -> begin
                match gradient_descent jacobian with
                | None -> D.split p jacobian
                | Some point -> begin
                    let vpl = D.to_vpl p in
                    let point' = Tools.VarMap.bindings point
                        |> List.map (
                        fun (var,coeff) ->
                            (Q.of_float coeff, D.to_ident var)
                        )
                        |> Vpl.Vector.Rat.Positive.mk
                    in
                    (*VPL.set_point point' vpl*)
                    VPL.get_regions (Some point') vpl
                    |> List.map D.of_vpl
                end
            end
            | _ -> D.split p jacobian
end

module VPL_Pizza = Make (struct
    include VPL
    let to_vpl x = x
    let of_vpl x = x
    let to_ident = Vpl_domain.Expr.Ident.toVar
    end)
