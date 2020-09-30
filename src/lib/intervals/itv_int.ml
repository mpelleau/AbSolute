open Bot

type bound = int

type t = bound * bound

(* not all pairs of integers are valid intervals *)
let validate ((l,h):t) : t =
  if l > h then invalid_arg  (Format.asprintf "itv_int.validate: %i %i" l h)
  else l,h

(* maps empty intervals to explicit bottom *)
let check_bot ((l,h):t) : t bot =
  if l <= h then Nb (l,h) else Bot

(************************************************************************)
(* CONSTRUCTORS AND CONSTANTS *)
(************************************************************************)

let of_bound (x:bound) : t = validate (x,x)

let of_bounds (l:bound) (h:bound) = validate (l,h)

let of_ints = of_bounds

(* of_floats (x,y) returns the biggest integer interval [n,m] s.t
   n >= x and m <= y, and n <= m
   maybe bottom if no such interval exists *)
let of_floats a b : t bot =
  check_bot ((int_of_float (ceil a)), (int_of_float (floor b)))

let of_int = of_bound

(*No integer interval can exactly abstract a single float *)
let of_float = Bot

let positive : t = (1,max_int)
let negative : t = (min_int,-1)

(************************************************************************)
(* PRINTING and CONVERSIONS *)
(************************************************************************)

let to_float_range ((a,b):t) = (float a), (float b)

let float_size ((a,b):t) = float (b - a)

let to_rational_range ((a,b):t) = (Mpqf.of_int a),(Mpqf.of_int b)

let print (fmt:Format.formatter) ((a,b):t) =
  if a = b then Format.print_int a
  else Format.fprintf fmt "[%i;%i]" a b

(************************************************************************)
(* SET-THEORETIC *)
(************************************************************************)

(* operations *)
(* ---------- *)
let join (l1,h1:t) (l2,h2:t) : t = (min l1 l2), (max h1 h2)
let meet (l1,h1:t) (l2,h2:t) : t bot = check_bot ((max l1 l2), (min h1 h2))

(* predicates *)
(* ---------- *)

(* contains_float (a,b) f, returns true if f can be converted exactly to
   an integer i and a <= i <= b *)
let contains_float ((a,b):t) f =
  let rounded = ceil f in
  rounded = f &&
    let rounded = int_of_float rounded in
    a <= rounded && rounded <= b

let intersect ((l1,h1):t) ((l2,h2):t) = l1 <= h2 &&  l2 <= h1

let is_singleton ((l,h):t) = l = h

(* mesure *)
(* ------ *)
let range ((a,b):t) = b - a

let score (a,b) = if b = a then 0. else 1./.float (b - a)

(* split *)
(* ----- *)

(* Split around the given number *)
let split_on ((a,b):t) (x : bound) =
  if x >= b || x <= a
  then [(a,b)]
  else match b-a with
  | 1 -> [(a,a); (b,b)]
  | 2 -> [(a,a); (a+1,a+1); (b,b)]
  | _ -> [(a,x); (x+1,b)]

(* splits in two, around the middle *)
let split ((a,b):t) = split_on (a,b) (a + (b-a)/2)

let prune (l1,u1:t) (l2,u2:t) : t list * t =
  match (l1 < l2),(u2 < u1) with
  | true , true  -> [(l1,(l2-1));((u2+1),u1)],(l2,u2)
  | true , false -> [(l1,(l2-1))],(l2,u1)
  | false, true  -> [((u2+1),u1)],(l1,u2)
  | false, false -> [],(l2,u2)

(************************************************************************)
(*              INTERVAL ARITHMETICS (FORWARD EVALUATION)               *)
(************************************************************************)

let neg (l,h:t) : t = -h, -l

let abs ((l,h) as i:t) : t =
  if l < 0 then 0,(max h (-l)) else i

let add (l1,h1:t) (l2,h2:t) : t =
  l1+l2, h1+h2

let sub (l1,h1:t) (l2,h2:t) : t =
  l1-h2, h1-l2

(* tries the different possibilities *)
let mix4 f l1 h1 l2 h2 =
  (min (min (f l1 l2) (f l1 h2)) (min (f h1 l2) (f h1 h2))),
  (max (max (f l1 l2) (f l1 h2)) (max (f h1 l2) (f h1 h2)))

let mul (l1,h1:t) (l2,h2:t) : t =
  mix4 ( * ) l1 h1 l2 h2

let div_sign (l1,h1) (l2,h2)  = mix4 ( / ) l1 h1 l2 h2

(* return valid values (possibly Bot) *)
let div (i1:t) (i2:t) : t bot =
  Format.printf "\n\n Integer division : %a / %a\n%!" print i1 print i2;
  (* split into positive and negative dividends *)
  let pos = (lift_bot (div_sign i1)) (meet i2 positive)
  and neg = (lift_bot (div_sign i1)) (meet i2 negative) in
  (* joins the result *)
  join_bot2 join pos neg

(* returns valid value when the exponant is a singleton positive integer.
   fails otherwise *)
let pow =
  let pow_aux i exp = int_of_float ((float i) ** (float exp)) in
  fun (l1,u1 as itv:t) (l2,u2:t) ->
  if l2=u2 then
    let exp = l2 in
    match exp with
    | 0 -> (1,1)
    | 1 -> itv
    | x when x > 1 ->
       if exp mod 2 = 1 then (pow_aux l1 exp),(pow_aux u1 exp)
       else
         if l1 >= 0 then
	         (pow_aux l1 exp),(pow_aux u1 exp)
         else if u1 <= 0 then
           (pow_aux u1 exp),(pow_aux l1 exp)
         else
           0,max (pow_aux l1 exp) (pow_aux u1 exp)
    | _ -> failwith "cant handle negatives powers"
  else failwith  "itv_int.ml cant handle non_singleton powers"

(* function calls (sqrt, exp, ln ...) are handled here :
   given a function name and and a list of argument,
   it returns a possibly bottom result *)
let eval_fun (_:string) (_:t list) : t bot =
  (*TODO: replace "assert false" with your own code *)
  assert false

(************************************************************************)
(* FILTERING (TEST TRANSFER FUNCTIONS)                                  *)
(************************************************************************)
let filter_leq (l1,h1:t) (l2,h2:t) : (t * t) Consistency.t =
  let open Consistency in
  if h1 <= l2 then Sat
  else if l1 > h2 then Unsat
  else Filtered (((l1, min h1 h2),(max l1 l2, h2)),false)

let filter_lt ((l1,h1):t) ((l2,h2):t) : (t * t) Consistency.t =
  let open Consistency in
  if h1 < l2 then Sat
  else if l1 >= h2 then Unsat
  else Filtered (((l1, min h1 (h2-1)),(max (l1+1) l2, h2)),false)

let filter_eq ((l1,h1):t) ((l2,h2):t) : t Consistency.t =
  let open Consistency in
  if l1=h1 && l2=h2 && l1 = l2 then Sat
  else
    let l = max l1 l2 and h = min h1 h2 in
    if l <= h then Filtered ((l,h),false)
    else Unsat

let filter_neq ((l1,h1) as i1:t) ((l2,h2) as i2:t) : (t * t) Consistency.t =
  let open Consistency in
  if l1=h1 && l2=h2 && l1 = l2 then Unsat
  else if intersect i1 i2 then Filtered ((i1,i2),false)
  else Sat

(* arithmetic *)
(* --------- *)

(* r = -i => i = -r *)
let filter_neg (i:t) (r:t) : t bot =
  meet i (neg r)

let filter_abs ((il,ih) as i:t) ((_,rh) as r:t) : t bot =
  if il >= 0 then meet i r
  else if ih <= 0 then meet i (neg r)
  else meet i (-rh, rh)

let filter_add (i1:t) (i2:t) (r:t) : (t*t) bot =
  merge_bot2 (meet i1 (sub r i2)) (meet i2 (sub r i1))

let filter_sub (i1:t) (i2:t) (r:t) : (t*t) bot =
  merge_bot2 (meet i1 (add i2 r)) (meet i2 (sub i1 r))

let filter_mul (i1:t) (i2:t) (r:t) : (t*t) bot =
  merge_bot2
    (if contains_float r 0. && contains_float i2 0. then Nb i1
     else strict_bot (meet i1) (div r i2))
    (if contains_float r 0. && contains_float i1 0. then Nb i2
     else strict_bot (meet i2) (div r i1))

let to_bexpr v ((l,h):t) = Csp_helper.inside v (Mpqf.of_int l) (Mpqf.of_int h)

(* returns the type annotation of the represented values *)
let to_annot _ = Csp.Int

(* filtering function calls like (sqrt, exp, ln ...) is done here :
     given a function name, a list of argument, and a result,
     it remove points that cannot satisfy the relation : f(arg1,..,argn) = r;
     it returns a possibly bottom result *)
let filter_fun (_:string) (_:t list) (_:t) : (t list) bot =
  (*TODO: replace "assert false" with your own code *)
  assert false

(* generate a random integer within the given interval *)
let spawn (l,h:t) : int =
  let r = Random.int ((h-l)+1) in
  l + r
