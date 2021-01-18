(** This module defines the different kinds of definition domains of variables *)

(** main type *)
type t =
  | Finite of Q.t * Q.t  (** \[a;b\] *)
  | Minf of Q.t  (** \]-oo; a\] *)
  | Inf of Q.t  (** \[a; +oo\[ *)
  | Set of Q.t list  (** \{x1; x2; ...; xn\} *)
  | Top  (** \]-oo; +oo\[ *)

(** {1 Constructors} *)

(** \[a;b\]*)
let interval inf sup = Finite (inf, sup)

(** finite sets of values \{x1; x2; ...; xn\} *)
let set l = Set l

(** semi-open interval : \]-oo;x\]*)
let minf x = Minf x

(** semi-open interval : \[x;+oo\[*)
let inf x = Inf x

(** \]-oo;+oo\[*)
let top = Top

(** {1 Predicates} *)

(** checks if a given rational belongs to a domain *)
let belong value = function
  | Finite (i, s) -> Mpqf.cmp i value <= 0 && Mpqf.cmp value s <= 0
  | Minf s -> Mpqf.cmp value s <= 0
  | Inf i -> Mpqf.cmp i value <= 0
  | Set l -> List.mem value l
  | Top -> true

(** {1 Operations} *)

(** Given a variable name [v] and a domain [d], builds the weakest constraint
    that [v] should respect to be in [d]. *)
let to_constraint v =
  let open Constraint in
  function
  | Finite (l, h) -> inside_cst v l h
  | Minf i -> leq (Var v) (Cst i)
  | Inf i -> geq (Var v) (Cst i)
  | Set (h :: tl) ->
      List.fold_left
        (fun acc e -> Constraint.Or (acc, assign v e))
        (assign v h) tl
  | _ -> eq Expr.one Expr.one

(** {1 Printing} *)
let print fmt = function
  | Finite (a, b) -> Format.fprintf fmt "[%a; %a]" Q.print a Q.print b
  | Minf i -> Format.fprintf fmt "[-oo; %a]" Q.print i
  | Inf i -> Format.fprintf fmt "[%a; +oo]" Q.print i
  | Set l ->
      let print_set =
        Format.pp_print_list
          ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
          Q.print
      in
      Format.fprintf fmt "{%a}" print_set l
  | Top -> Format.fprintf fmt "[-oo; +oo]"
