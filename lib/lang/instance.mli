(** This module defines the type of points (also called instances), i.e mappings
    from variables to rational coordinates. *)

(** the instance type *)
type t = Q.t Tools.VarMap.t

(** {1 Constructors} *)

val of_list : (string * Mpqf.t) list -> t
(** Builds an instance from a list of bindings *)

(** {1 Operations} *)

val translate : t -> t -> t
(** Translation of an instance by a vector. [translate i v] builds the point
    corresponding the translation of i by v. Useful for gradient descent
    techniques *)

val to_apron_gen : t -> Apron.Generator1.t
(** conversion to apron generator *)

(** {1 Printing} *)

val print : Format.formatter -> t -> unit
(** printer *)

val to_string : t -> string
(** Conversion to a string *)
