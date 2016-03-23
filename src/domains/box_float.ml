module Env = Map.Make (struct type t = Syntax.var let compare = compare end)

module I = Itv.ItvF
module B = I.B

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

let is_bottom (abs:t) = Env.for_all (fun k (_,v) -> I.check_bot v <> Bot.Bot) abs

let is_small (abs:t) = Env.for_all (fun k (_,((b1,b2) as v)) -> 
  not (I.is_bounded v) || 
  B.sub_up b2 b1 > B.of_float_up !Constant.precision
) abs
