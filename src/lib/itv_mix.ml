open Bot

module I = Itv_int
module R = Itv.ItvF

type t = Int of I.t | Real of R.t

(* useful constructors *)
let make_real x = Real x
let make_int x  = Int x

(* Conversion utilities *)
(************************)

(* integer itv conversion to real itv *)
let to_float (i:I.t) =
  let (a,b) = I.to_float_range i in
  R.of_floats a b

(* real itv conversion to integer itv.
   the biggest inner element is returned.
   may be bottom *)
let to_int (r:R.t) : I.t bot =
  let (a,b) = R.to_float_range r in
  match classify_float a, classify_float b with
  | (FP_normal | FP_zero), (FP_normal | FP_zero) -> I.of_floats a b
  | _ -> Tools.fail_fmt "problem with to_int conversion of : %a" R.print r

(* force conversion to real itv *)
let force_real (i:t) : R.t =
  match i with
  | Real r -> r
  | Int i -> to_float i

let dispatch f_int f_real = function
  | Int x -> f_int x
  | Real x -> f_real x

let map f_int f_real = function
  | Int x -> Int (f_int x)
  | Real x -> Real (f_real x)

(************************************************************************)
(*                      CONSTRUCTORS AND CONSTANTS                      *)
(************************************************************************)

let of_ints (x1:int) (x2:int) : t =
  Int(I.of_ints x1 x2)

let of_floats (x1:float) (x2:float) : t =
  Real(R.of_floats x1 x2)

let of_int (x1:int) : t =
  of_ints x1 x1

let of_float (x1:float) : t =
  of_floats x1 x1

let of_rats (m1:Mpqf.t) (m2:Mpqf.t) : t =
  Real(R.of_rats m1 m2)

let of_rat (m1:Mpqf.t) : t =
  of_rats m1 m1

(* maps empty intervals to explicit bottom *)
let check_bot (x:t) : t bot =
  match x with
  | Int x -> lift_bot make_int (I.check_bot x)
  | Real x -> lift_bot make_real (R.check_bot x)

(* unbounded interval constructor *)
let top_int : t = of_ints min_int max_int (*TODO: improve soundness*)

let top_real : t = Real (R.top_real)

(************************************************************************)
(*                       PRINTING and CONVERSIONS                       *)
(************************************************************************)

let to_float_range (x:t) : float * float =
  dispatch I.to_float_range R.to_float_range x

let to_rational_range (x:t) : Mpqf.t * Mpqf.t =
  dispatch I.to_rational_range R.to_rational_range x

let print (fmt:Format.formatter) (x:t) : unit =
  dispatch (Format.fprintf fmt "%a" I.print) (Format.fprintf fmt "%a" R.print) x

let float_size (i:t) : float =
  dispatch I.float_size R.float_size i

(************************************************************************)
(* SET-THEORETIC *)
(************************************************************************)

(* operations *)
(* ---------- *)

let join (x1:t) (x2:t) : t =
  match x1,x2 with
  | Int x1, Int x2 -> Int (I.join x1 x2)
  | Real x1, Real x2 -> Real (R.join x1 x2)
  | Int x1, Real x2 | Real x2, Int x1 -> Real (R.join (to_float x1) x2)

let meet (x1:t) (x2:t) : t bot =
  match x1,x2 with
  | Int x1, Int x2 -> lift_bot make_int (I.meet x1 x2)
  | Real x1, Real x2 -> lift_bot make_real (R.meet x1 x2)
  | Int x1, Real x2 | Real x2, Int x1 ->
     let x1 = to_float x1 in
     lift_bot make_int (strict_bot to_int (R.meet x1 x2))

(* predicates *)
(* ---------- *)

(* retruns true if the interval is positive (large sense), false otherwise *)
let is_positive (itv:t) : bool =
  match itv with
  | Int (a,b) -> a >= 0
  | Real r -> R.subseteq r R.positive

(* retruns true if the interval is negative (large sense), false otherwise *)
let is_negative (itv:t) : bool =
  match itv with
  | Int (a,b) -> b <= 0
  | Real r -> R.subseteq r R.negative

let contains_float (x:t) (f:float) : bool =
  match x with
  | Int x ->  I.contains_float x f
  | Real x -> R.contains x f

let intersect (x1:t) (x2:t) : bool =
  match x1,x2 with
  | Int x1, Int x2 -> I.intersect x1 x2
  | Real x1, Real x2 -> R.intersect x1 x2
  | Int x1, Real x2 | Real x2, Int x1 ->
     match to_int x2 with
     | Bot -> false
     | Nb x2 -> I.intersect x1 x2

let is_singleton (itv:t) : bool =
  dispatch I.is_singleton R.is_singleton itv

(* mesure *)
(* ------ *)
let range (x:t) : float =
  match x with
  | Int x -> float (I.range x)
  | Real x -> R.range x

let score = dispatch I.score R.score

(* split *)
(* ----- *)
let split (x:t) : t list =
  match x with
  | Real x -> R.split x |> List.map make_real
  | Int x -> I.split x  |> List.map make_int

let split_on (x:t) (value : Mpqf.t) : t list =
  match x with
  | Real x -> R.split_on x value |> List.map make_real
  | Int x -> I.split_on x (Mpqf.to_float value |> int_of_float) |> List.map make_int

(* pruning *)
(* ------- *)
let prune (x1:t) (x2:t) : t list * t =
  (* TODO: replace the "failwith" with your own code *)
  failwith "function 'prune' in file 'itv_mix.ml' not implemented"

(************************************************************************)
(* INTERVAL ARITHMETICS (RORWARD EVALUATION) *)
(************************************************************************)

let neg (x:t) : t =
  (* Format.printf "neg : - %a = " print x;
   * let res = *)
    map I.neg R.neg x
  (* in
   * Format.printf "%a\n%!" print res;
   * res *)

let abs (x:t) : t =
  map I.abs R.abs x

let add (x1:t) (x2:t) : t =
  (* Format.printf "add : %a + %a = " print x1 print x2;
   * let res = *)
    match x1,x2 with
  | Int x1 , Int x2 ->  Int(I.add x1 x2)
  | Real x1, Real x2 -> Real (R.add x1 x2)
  | Int x1, Real x2 | Real x2, Int x1 -> Real (R.add x2 (to_float x1))
  (* in
   * Format.printf "%a\n%!" print res;
   * res *)

let sub (x1:t) (x2:t) : t =
  (* Format.printf "sub : %a - %a = " print x1 print x2;
   * let res = *)
    match x1,x2 with
    | Int x1 , Int x2  -> Int  (I.sub x1 x2)
    | Real x1, Real x2 -> Real (R.sub x1 x2)
    | Int x1 , Real x2 -> Real (R.sub (to_float x1) x2)
    | Real x1, Int x2  -> Real (R.sub x1 (to_float x2))
  (* in
   * Format.printf "%a\n%!" print res;
   * res *)

let mul (x1:t) (x2:t) : t =
  (* Format.printf "mul : %a * %a = " print x1 print x2;
   * let res = *)
    match x1,x2 with
    | Int x1 , Int x2 ->  Int (I.mul x1 x2)
    | Real x1, Real x2 -> Real (R.mul x1 x2)
    | Int x1, Real x2 | Real x2, Int x1 -> Real (R.mul (to_float x1) x2)
  (* in
   * Format.printf "%a\n%!" print res;
   * res *)

(* return valid values (possibly Bot) *)
let div (x1:t) (x2:t) : t bot =
  let x1 = force_real x1 in
  match x2 with
  | Int x2 ->
     if I.contains_float x2 0. then
       let pos = I.meet I.positive x2 and neg = I.meet I.negative x2 in
       let divpos = strict_bot (fun x -> R.div x1 (to_float x)) pos
       and divneg = strict_bot (fun x -> R.div x1 (to_float x)) neg
       in lift_bot make_real (join_bot2 R.join divpos divneg)
     else lift_bot make_real (R.div x1 (to_float x2))
  | Real x2 -> lift_bot make_real (R.div x1 x2)

(* returns valid value when the exponant is a singleton positive integer.
     fails otherwise *)
let pow (x1:t) (x2:t) : t =
  match x1,x2 with
  | Int x1, Int x2 -> Int (I.pow x1 x2)
  | Real x1, Real x2 -> Real (R.pow x1 x2)
  | Int x1, Real x2 -> Real (R.pow (to_float x1) x2)
  | Real x1, Int x2 -> Real (R.pow x1 (to_float x2))

(* nth-root *)
let n_root (i1:t) (i2:t) =
  (*TODO: maybe improve precision on perfect nth roots *)
  let i1,i2 = (force_real i1),(force_real i2) in
  lift_bot make_real (R.n_root i1 i2)

(* function calls (sqrt, exp, ln ...) are handled here :
   given a function name and and a list of argument,
   it returns a possibly bottom result *)
let eval_fun (name:string) (args:t list) : t bot =
  let args = List.map (function Real x -> x | Int x -> (to_float x)) args in
  lift_bot make_real (R.eval_fun name args)

(************************************************************************)
(* FILTERING (TEST TRANSRER RUNCTIONS) *)
(************************************************************************)

(* given two interval arguments, return a subset of each argument
   by removing points that cannot satisfy the predicate;
   may also return Bot if no point can satisfy the predicate *)
let filter_leq (x1:t) (x2:t) : (t * t) bot =
  (* Format.printf "filter_leq : %a <= %a => " print x1 print x2; *)
  match x1,x2 with
  | Int x1 , Int x2 ->  lift_bot (fun (x,y) -> (Int x),(Int y)) (I.filter_leq x1 x2)
  | Real x1, Real x2 -> lift_bot (fun (x,y) -> (Real x),(Real y)) (R.filter_leq x1 x2)
  | Int x1, Real x2 ->
     let x1 = to_float x1 in
     (match R.filter_leq x1 x2 with
     | Bot -> Bot
     | Nb (x1,x2) ->
        lift_bot (fun x1 -> Int x1, Real x2) (to_int x1))
  | Real x1, Int x2 ->
     let x2 = to_float x2 in
     (match R.filter_leq x1 x2 with
     | Bot -> Bot
     | Nb (x1,x2) ->
        lift_bot (fun x2 -> Real x1, Int x2) (to_int x2))

let filter_lt (x1:t) (x2:t) : (t * t) bot =
  (* Format.printf "filter_lt : %a < %a => " print x1 print x2; *)
  match x1,x2 with
  | Int x1 , Int x2 ->  lift_bot (fun (x,y) -> (Int x),(Int y)) (I.filter_lt x1 x2)
  | Real x1, Real x2 -> lift_bot (fun (x,y) -> (Real x),(Real y)) (R.filter_lt x1 x2)
  | Int x1, Real x2 ->
     let x1 = to_float x1 in
     (match R.filter_lt x1 x2 with
      | Bot -> Bot
      | Nb (x1,x2) ->
        lift_bot (fun x1 -> Int x1, Real x2) (to_int x1))
  | Real x1, Int x2 ->
     let x2 = to_float x2 in
     (match R.filter_lt x1 x2 with
     | Bot -> Bot
     | Nb (x1,x2) ->
        lift_bot (fun x2 -> Real x1, Int x2) (to_int x2))

let filter_eq (x1:t) (x2:t) : (t * t) bot =
  (* Format.printf "filter_eq : %a = %a => " print x1 print x2;
   * let res = *)
  match x1,x2 with
  | Int x1 , Int x2 ->  lift_bot (fun (x,y) -> (Int x),(Int y)) (I.filter_eq x1 x2)
  | Real x1, Real x2 -> lift_bot (fun (x,y) -> (Real x),(Real y)) (R.filter_eq x1 x2)
  | Int x1, Real x2 ->
     let x1 = to_float x1 in
     (match R.filter_eq x1 x2 with
     | Bot -> Bot
     | Nb (x1,x2) ->
        lift_bot (fun x1 -> Int x1, Real x2) (to_int x1))
  | Real x1, Int x2 ->
     let x2 = to_float x2 in
     (match R.filter_eq x1 x2 with
     | Bot -> Bot
     | Nb (x1,x2) ->
        lift_bot (fun x2 -> Real x1, Int x2) (to_int x2))
  (* in
   * match res with
   * | Nb (r1,r2) -> Format.printf "%a, %a \n%!" print r1 print r2; res
   * | Bot -> Format.printf "bottom"; res *)

let filter_neq (i1:t) (i2:t) : (t * t) bot =
  (* Format.printf "filter: %a <> %a\n%!" print i1 print i2; *)
  match i1,i2 with
  | Int x1 , Int x2    -> lift_bot (fun (x,y) -> (Int x),(Int y)) (I.filter_neq x1 x2)
  | Real x1, Real x2   -> lift_bot (fun (x,y) -> (Real x),(Real y)) (R.filter_neq x1 x2)
  (* Special handling of singleton to add precision *)
  | Real x1, Int (a,b) ->
     let r1,r2 = R.to_float_range x1 in
     if r1=r2 then
       if float a = r1 then
         let new_i2 = I.check_bot ((a+1),b) in
         lift_bot (fun (x,y) -> (Real x),(Int y)) (lift_bot (fun x2->x1,x2) new_i2)
       else if float b = r1 then
         let new_i2 = I.check_bot (a,b-1) in
         lift_bot (fun (x,y) -> (Real x),(Int y)) (lift_bot (fun x2->x1,x2) new_i2)
       else Nb (i1,i2)
     else Nb (i1,i2)
  | Int (a,b),Real x1 ->
     let r1,r2 = R.to_float_range x1 in
     if r1=r2 then
       if float a = r1 then
         let new_i2 = I.check_bot ((a+1),b) in
         lift_bot (fun (x,y) -> (Int x),(Real y)) (lift_bot (fun x2->x2,x1) new_i2)
       else if float b = r1 then
         let new_i2 = I.check_bot (a,b-1) in
         lift_bot (fun (x,y) -> (Int x),(Real y)) (lift_bot (fun x2->x2,x1) new_i2)
       else Nb (i1,i2)
     else Nb (i1,i2)
(* given the interval argument(s) and the expected interval result of
   a numeric operation, returns a refined interval argument(s) where
   points that cannot contribute to a value in the result are
   removed;
   may also return Bot if no point in an argument can lead to a
   point in the result *)

let filter_neg (i:t) (r:t) : t bot =
  meet i (neg r)

let filter_abs (i:t) (r:t) : t bot =
  if is_positive i then meet i r
  else if is_negative i then meet i (neg r)
  else meet i (join (neg r) r)

let filter_add (i1:t) (i2:t) (r:t) : (t*t) bot =
  merge_bot2 (meet i1 (sub r i2)) (meet i2 (sub r i1))

(* r = i1-i2 => i1 = i2+r /\ i2 = i1-r *)
let filter_sub (i1:t) (i2:t) (r:t) : (t*t) bot =
  merge_bot2 (meet i1 (add i2 r)) (meet i2 (sub i1 r))

(* r = i1*i2 => (i1 = r/i2 \/ i2=r=0) /\ (i2 = r/i1 \/ i1=r=0) *)
let filter_mul (i1:t) (i2:t) (r:t) : (t*t) bot =
  (* Format.printf "filter_mul : %a * %a = %a => " print i1 print i2 print r;
   * let res = *)
  merge_bot2
    (if contains_float r 0. && contains_float i2 0. then Nb i1
     else match div r i2 with Bot -> Bot | Nb x -> meet i1 x)
    (if contains_float r 0. && contains_float i1 0. then Nb i2
     else match div r i1 with Bot -> Bot | Nb x -> meet i2 x)
  (* in
   * match res with
   * | Nb (r1,r2) -> Format.printf "%a, %a \n%!" print r1 print r2; res
   * | Bot -> Format.printf "bottom\n%!"; res *)

(* r = i1/i2 => i1 = i2*r /\ (i2 = i1/r \/ i1=r=0) *)
let filter_div (i1:t) (i2:t) (r:t) : (t*t) bot =
  (* Format.printf "filter_div : %a / %a => " print i1 print i2;
   * let res = *)
    merge_bot2
    (meet i1 (mul i2 r))
    (if contains_float r 0. && contains_float i1 0. then Nb i2
     else match (div i1 r) with Bot -> Bot | Nb x -> meet i2 x)
  (* in
   * match res with
   * | Nb (r1,r2) -> Format.printf "%a, %a \n%!" print r1 print r2; res
   * | Bot -> Format.printf "bottom\n%!"; res *)

let filter_pow (i:t) (n:t) (r:t) : (t*t) bot =
  merge_bot2 (meet_bot meet i (n_root r n)) (Nb n)


(* filtering first operand only *)
(********************************)

(* r = i + c => i = r - c *)
let filter_add_f (i:t) (c:t) (r:t) : t bot =
  meet i (sub r c)

(* r = i - c => i = r + c *)
let filter_sub_f (i:t) (c:t) (r:t) : t bot =
  meet i (add c r)

(* r = i*c => (i = r/c \/ c=r=0) *)
let filter_mul_f (i:t) (c:t) (r:t) : t bot =
  if contains_float r 0. && contains_float c 0. then Nb i
  else match div r c with Bot -> Bot | Nb x -> meet i x

(* r = i/c => i = r*c *)
let filter_div_f (i:t) (c:t) (r:t) : t bot =
  meet i (mul c r)

(* r = i ** n => i = nroot r *)
let filter_pow_f (i:t) n (r:t) =
  meet_bot meet i (n_root r n)

(* r = nroot i => i = r ** n *)
let filter_root_f i r n =
  meet i (pow r n)

let to_expr (itv:t) =
  dispatch I.to_expr R.to_expr itv

(* returns the type annotation of the represented values *)
let to_annot x =
  dispatch I.to_annot R.to_annot x

(* filtering function calls like (sqrt, exp, ln ...) is done here :
   given a function name, a list of argument, and a result,
   it remove points that cannot satisfy the relation : f(arg1,..,argn) = r;
   it returns a possibly bottom result *)
let filter_fun (name:string) (args:t list) (res:t) : (t list) bot =
  let args = List.map (function Real x -> x | Int x -> (to_float x)) args in
  let float_res = match res with Real x -> x | Int x -> to_float x in
  lift_bot (List.map make_real) (R.filter_fun name args float_res)

(* generate a random float within the given interval *)
let spawn (x:t) : float =
  match x with
  | Int x  -> float (I.spawn x)
  | Real x -> R.spawn x

let shrink (i : t) (c:Mpqf.t) : t bot =
  match i with
  | Int i -> I.shrink i c |> lift_bot make_int
  | Real i -> R.shrink i c |> lift_bot make_real
