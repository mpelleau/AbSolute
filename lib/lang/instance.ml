(** This module defines the type of points (also called instances), i.e mappings
    from variables to rational coordinates. *)

open Tools

(** the instance type *)
type t = Q.t VarMap.t

(** {1 Constructors} *)

(** Builds an instance from a list of bindings *)
let of_list : (string * Q.t) list -> t = VarMap.of_list

(** {1 Operations} *)

(** Translation of an instance by a vector. [translate i v] builds the point
    corresponding the translation of i by v. Useful for gradient descent
    techniques *)
let translate i1 i2 =
  VarMap.fold (fun v q acc -> VarMap.update v (Option.map (Q.add q)) acc) i1 i2

(** {1 Printing} *)

(** printer *)
let print fmt instance =
  let bind fmt (var, value) =
    Format.fprintf fmt "%s : %a" var Q.pp_print value
  in
  VarMap.bindings instance
  |> Format.fprintf fmt "{%a}" (Format.pp_print_list ~pp_sep:newline_sep bind)

(** Conversion to a string *)
let to_string : t -> string = Format.asprintf "%a" print
