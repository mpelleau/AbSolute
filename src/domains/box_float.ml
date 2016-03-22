module Env = Map.Make (struct type t = Syntax.var let compare = compare end)

module I = Itv.ItvF

type t =  (Syntax.typ * I.t) Env.t

(* Printing *)
let print fmt a =
  let first = ref true in
  Env.iter
    (fun v (t,i) ->
      Format.fprintf fmt "%s%a %s:%a" 
	(if !first then "" else " ") 
	Syntax.print_typ t 
	v
	I.print i;
      first := false
    ) a

let is_integer (abs:t) var = fst (Env.find var abs) = Syntax.INT

(* total number of variables *)
let dimension (a:t) : int = Env.cardinal a
