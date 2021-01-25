type t =
  | Finite of Q.t * Q.t
  | Minf of Q.t
  | Inf of Q.t
  | Set of Q.t list
  | Top

let interval inf sup = Finite (inf, sup)

let set l = Set l

let minf x = Minf x

let inf x = Inf x

let top = Top

let belong (value : Q.t) = function
  | Finite (i, s) -> Mpqf.cmp i value <= 0 && Mpqf.cmp value s <= 0
  | Minf s -> Mpqf.cmp value s <= 0
  | Inf i -> Mpqf.cmp i value <= 0
  | Set l -> List.mem value l
  | Top -> true

let is_bounded = function Finite _ | Set _ -> true | _ -> false

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
  | _ -> eq Expr.zero Expr.zero

(** {1 Printing} *)
let print fmt = function
  | Finite (a, b) -> Format.fprintf fmt "[%a; %a]" Q.print a Q.print b
  | Minf i -> Format.fprintf fmt "[-oo; %a]" Q.print i
  | Inf i -> Format.fprintf fmt "[%a; +oo]" Q.print i
  | Set l -> Format.fprintf fmt "{%a}" (Tools.pp_list_sep "; " Q.print) l
  | Top -> Format.fprintf fmt "[-oo; +oo]"

(** Conversion to a string *)
let to_string : t -> string = Format.asprintf "%a" print
