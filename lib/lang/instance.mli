(** This module defines the type of points (also called instances), i.e mappings
    from variables to rational coordinates. *)

(** the type of (untyped) instances *)
type t = Q.t Tools.VarMap.t

(** the type of typed instances *)
type typed = (Q.t * Tools.typ) Tools.VarMap.t

(** {1 Constructors} *)

val of_list : (string * Mpqf.t) list -> t
(** Builds an instance from a list of bindings *)

val of_list_typed : (string * (Mpqf.t * Tools.typ)) list -> typed
(** Builds an instance from a list of bindings *)

(** {1 Operations} *)

val translate : t -> t -> t
(** Translation of an instance by a vector. [translate i v] builds the point
    corresponding the translation of i by v. Useful for gradient descent
    techniques *)

val iter : (string -> Q.t -> unit) -> t -> unit
(** iterates through variablez and their coordinante **)

val to_seq : t -> (string * Q.t) Seq.t
(** conversion to sequence **)

val iter_typed : (string -> Q.t * Tools.typ -> unit) -> typed -> unit
(** iterates through variablez and their coordinante **)

val to_seq_typed : typed -> (string * (Q.t * Tools.typ)) Seq.t
(** conversion to sequence **)

val to_apron_gen : t -> Apron.Generator1.t
(** conversion to apron generator *)

(** {1 Printing} *)

val print : Format.formatter -> t -> unit
(** printer *)

val to_string : t -> string
(** Conversion to a string *)

val print_typed : Format.formatter -> typed -> unit
(** printer *)

val to_string_typed : typed -> string
(** Conversion to a string *)

val untype : typed -> t
(** conversion from untyped to typed *)
