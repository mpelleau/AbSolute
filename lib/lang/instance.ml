open Tools

(** the instance type *)
type t = Q.t VarMap.t

let print fmt instance =
  let bind fmt (var, value) =
    Format.fprintf fmt "%s : %a" var Q.pp_print value
  in
  VarMap.bindings instance
  |> Format.fprintf fmt "{%a}" (Format.pp_print_list ~pp_sep:newline_sep bind)
