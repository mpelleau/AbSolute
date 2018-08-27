(*
  This modules defines sound operators for trogonometrical functions
  It uses floating point precision
  It plugs itself over a interval arithmetic module
  The interface is functional.
*)

open Bot

module F = Bound_float

module Make (I:Itv_sig.ITV) = struct

  (* All the classical interval computations are keeped *)
  include I


  (********************)
  (* PI APPROXIMATION *)
  (********************)

  (* pi approximation (double precision) *)

  let pi_down = 3.14159265358979312 (* closest smaller float than pi *)
  (* real pi  = 3.141592653589793238462.......... *)
  let pi_up   = 3.14159265358979356 (* closest bigger float than pi *)

  let pi_itv = pi_down,pi_up

  (*********************)
  (* SIN APPROXIMATION *)
  (*********************)

  (* over-approximation of sin x *)
  let sin_up x = sin x

  (* under-approximation of sin x *)
  let sin_down x =
    let a = sin (x+.pi_up) and b =  sin (x+.pi_down) in
    -. (max a b)

  (*********************)
  (* COS APPROXIMATION *)
  (*********************)

  (* over-approximation of cos x *)
  let cos_up x = cos x

  (* under-approximation of cos x *)
  let cos_down x =
    let a = cos (x+.pi_up) and b =  cos (x+.pi_down) in
    -. (max a b)

  (*********************)
  (* TAN APPROXIMATION *)
  (*********************)

  (* TODO: check division by zero *)

  (* over-approximation of tan x *)
  let tan_up x = tan x

  (* under-approximation of tan x *)
  let tan_down x =  -. (tan (-. x))

  (************************)
  (* ARCTAN APPROXIMATION *)
  (************************)

  let atan_down x = atan x

  let atan_up x = -. (atan (-.x))

  (************************)
  (* ARCCOS APPROXIMATION *)
  (************************)

  let acos_up r =
    if -1. <= r && r <= 1. then Nb (acos r)
    else Bot

  let acos_down r =
    if -1. <= r && r <= 1. then Nb (-. (acos (-.r)))
    else Bot

  (************************)
  (* ARCSIN APPROXIMATION *)
  (************************)

  let asin_up r =
    if -1. <= r && r <= 1. then Nb (asin r)
    else Bot

  let asin_down r =
    if -1. <= r && r <= 1. then Nb (-. (asin (-.r)))
    else Bot

  (****************************************************)
  (*               INTERVAL COMPUTATION               *)
  (****************************************************)

  type float_itv = float * float

  let print_fitv fmt (l,u) = Format.fprintf fmt
                                       "[%a; %a]"
                                       Format.pp_print_float l
                                       Format.pp_print_float u

  let pihalf_down   = F.div_down pi_down F.two
  let pihalf_up     = F.div_up pi_up F.two
  let twopiup       = F.mul_up F.two pi_up
  let twopidown     = F.mul_down F.two pi_down

  let pihalf_fitv    = (pihalf_down, pihalf_up)
  let pi_fitv        = (F.of_float_down pi_down), (F.of_float_up pi_up)
  let twopi_fitv     = (twopidown, twopiup)

  let fitv_to_i (f1,f2) = I.of_floats f1 f2

  let pihalf_itv = fitv_to_i pihalf_fitv
  let pi_itv     = fitv_to_i pi_fitv
  let twopi_itv  = fitv_to_i twopi_fitv

  (* the type of monotony of a function on a given interval: *)
  (* - Incr means strictly increasing*)
  (* - Decr means stricly decreasing*)
  (* - Change means than the function f is not monotonic on the given interval.
       the boolean indicates if the function is firstly increasing (true)
       or decreasing(false) *)
  type monotony = Incr
                | Decr
                | Change of bool

  let print_monotony fmt mon =
    let s =
      match mon with
      | Incr -> "increasing"
      | Decr -> "decreasing"
      | Change true -> "increasing then changes"
      | Change false -> "decreasing then changes"
    in
    Format.fprintf fmt "%s" s

  (* given an interval i, and a function f and its monotony,
   return the image of i by f *)
  let itv monotony f_down f_up (a,b) =
    match monotony with
    | Incr     -> (f_down a),(f_up b)
    | Decr     -> (f_down b),(f_up a)
    | Change i ->
       if i then
         let a' = f_down a and b'= f_down b in
         (min a' b',1.)
       else
         let a' = f_up a and b'= f_up b in
         (-1.),(max a' b')

  (***************************************************)
  (* INTERVAL EVALUTATION OF TRIGONOMETRIC FUNCTIONS *)
  (***************************************************)

  let cosmonotony (a,b) =
    (* returns the monotony, supposes that the monotony changes at most once *)
    let mono_once (a,b) =
      let m_sin_a = -. (sin a) and m_sin_b = -. (sin b) in
      if m_sin_a = 0. then
        if m_sin_b < 0. then Decr else Incr
      else
      if m_sin_a < 0. then
        if m_sin_b <= 0. then Decr
        else Change(false)
      else
        if m_sin_b >= 0. then Incr
        else Change(true)
    in
    let rec mono (a,b) =
      if a > b then assert false;
      let range = (F.sub_up b a) in
      if twopidown <= range then raise Exit
      else
        if pi_down <= range then
          (* monotony changes at least once, at most twice *)
          let mid = a +. (b-.a)/.2. in
          match (mono (a,mid)),(mono (mid,b)) with
          | Incr,Incr -> Incr
          | Decr,Decr -> Decr
          | Incr,Decr -> Change (true)
          | Decr,Incr -> Change (false)
          | Change (_), Change (_) -> raise Exit
          | (Change (_) as x), _ | _,(Change (_) as x) -> x
        else
          (* monotony changes at most once *)
          mono_once (a,b)
    in mono (a,b)

  (* cosinus of an interval *)
  let cos_itv i =
    let (a,b) = I.to_float_range i in
    match cosmonotony (a,b) with
    | mon ->
       (* Format.printf "monotony is %a on %a\n%!" print_monotony mon print_fitv (a,b); *)
       fitv_to_i (itv mon cos_down cos_up (a,b))
    | exception Exit -> I.of_floats (-1.) 1.

  (* interval acos *)
  let acos_itv i =
    let (l,u) = I.to_float_range i in
    if 1. < l ||  u < -1. then Bot
    else
      let l' = if 1. < u then 0. else debot (acos_down u)
      and u' =  if l < -1. then pi_up else debot (acos_up l)
      in Nb(I.of_floats l' u')

  (* sinus of an interval *)
  let sin_itv i =
    cos_itv (I.sub i pihalf_itv)

  (* interval asin (arcos + arcsin = pi/2) *)
  let asin_itv i = lift_bot (I.sub pihalf_itv) (acos_itv i)

  (* tangent of an interval *)
  let tan_itv i = (I.div (sin_itv i) (cos_itv i))

  (* atan of an interval *)
  let atan_itv i =
    let (l,u) = I.to_float_range i in
    fitv_to_i ((atan_down l),(atan_up u))

  (* we augment the function evaluator to add to it the trigonometrical stuff *)
  let eval_fun name args =
    let arity_1 (f: I.t -> I.t) : I.t bot =
      match args with
      | [i] -> Nb (f i)
      | _ -> Tools.fail_fmt "%s expect one argument" name
    in
    let arity_1_bot (f: I.t -> I.t bot) : I.t bot =
      match args with
      | [i] -> f i
      | _ -> Tools.fail_fmt "%s expect one argument" name
    in
    match name with
    | "cos"  -> arity_1 cos_itv
    | "sin"  -> arity_1 sin_itv
    | "acos" -> arity_1_bot acos_itv
    | "asin" -> arity_1_bot asin_itv
    | "tan"  -> arity_1_bot (fun x -> fst (tan_itv x))
    | "atan" -> arity_1 atan_itv
    | _ -> I.eval_fun name args

  (***************************************)
  (* FILTERING (TEST TRANSFER FUNCTIONS) *)
  (***************************************)

  (* bring an interval to target (the lower bound )
     interval size should be smaller than maxsize, it raises Exit if not *)
  let normalize target maxsize i =
    if maxsize <= I.float_size i then raise Exit
    else
      let (a,b) = I.to_float_range i in
      let nb = floor (F.div_down (a-.target) maxsize) in
      let dist = I.mul (I.of_float nb) (I.of_float maxsize) in
      let i' = I.sub i dist in
      (* the interval can grow during the normalization so we have to recheck *)
      if maxsize <= I.float_size i' then raise Exit
      else i',dist

  (* general function for both arcsin and arcos *)
  let arc return_range fun_itv =
    let other = I.add return_range pi_itv in
    fun itv result ->
    (* handling of the symetry *)
    match (I.meet itv return_range),(I.meet itv other) with
    | Bot,Bot -> Bot
    | Nb _,Bot  -> fun_itv result
    | Bot,Nb _  -> lift_bot (I.add pi_itv) (fun_itv (I.neg result))
    | Nb _,Nb _   ->
       lift_bot (I.add pi_itv) (fun_itv (I.neg result))
       |> join_bot2 I.join (fun_itv result)

  (* 0 < x < 2pi && cos(x) = r <=> x = arcos r || x = arcos(-r)+pi *)
  let arcos_0_2pi = arc (I.of_floats 0. pi_up) acos_itv

  (* -pi/2 < x < 3pi/2 && sin(x) = r <=> x = arcsin r *)
  let arcsin_mpih_pih = arc (I.sub (I.of_floats 0. pi_up) pihalf_itv) asin_itv

  (* general function for both filter_sin and filter_cos *)
  let filter domain_range fun_itv =
    let other = I.add twopi_itv domain_range in
    fun (i:I.t) (r:I.t) : I.t bot ->
    try
      let i',delta  = normalize 0. twopiup i in
      let first_part =
        match (I.meet i' domain_range) with
        | Bot -> Bot
        | Nb i' -> lift_bot (I.add delta) (fun_itv i' r)
      in
      let second_part =
        match (I.meet i' other) with
        | Nb x ->
           let x' = I.sub x twopi_itv in
           lift_bot (I.add (I.add delta twopi_itv)) (fun_itv x' r)
        | Bot -> Bot
      in
      join_bot2 I.join first_part second_part
    with
    | Exit -> Nb i

  (* r = cos i => i mod 2pi = arccos r *)
  let filter_cos =
    filter (I.of_floats 0. twopiup) arcos_0_2pi

  (* r = sin i => i mod 2pi = arcsin r *)
  let filter_sin =
    filter (I.sub (I.of_floats 0. twopiup) pihalf_itv) arcsin_mpih_pih

  (* -pi/2 < x < 3pi/2 && tan(x) = r <=> x = arctan r *)
  let arctan_mpih_pih =
    arc (I.sub (I.of_floats 0. pi_up) pihalf_itv) (fun i -> Nb (atan_itv i))

  (* r = tan i => i = artan r) => *)
  let filter_tan (i:I.t) (r:I.t) : I.t bot =
    try
      let i',delta = normalize (-. pihalf_up) pi_up i in
      lift_bot (I.add delta) (I.meet i' (atan_itv r))
    with
    | Exit -> Nb i

  (* r = asin i => i = sin r *)
  let filter_asin i r =
    I.meet i (sin_itv r)

  (* r = acos i => i = cos r *)
  let filter_acos i r =
    I.meet i (cos_itv r)

  (* r = atan i => i = tan r *)
  let filter_atan i r =
    I.meet i (atan_itv r)

  (* we augment the function filterer to add to it the trigonometrical stuff *)
  let filter_fun name args r : (I.t list) bot =
    let arity_1 (f: I.t -> I.t -> I.t bot) : (I.t list) bot =
      match args with
      | [i] ->
         (match f i r with
         | Bot -> Bot
         | Nb i -> Nb [i])
      | _ -> Tools.fail_fmt "%s expect one argument" name
    in
    match name with
    | "cos"  -> arity_1 filter_cos
    | "sin"  -> arity_1 filter_sin
    | "acos" -> arity_1 filter_acos
    | "asin" -> arity_1 filter_asin
    | "tan"  -> arity_1 filter_tan
    | "atan" -> arity_1 filter_atan
    | _ -> I.filter_fun name args r
end

module ItvF = Make(Itv.ItvF)
(* module ItvMix = Make(Itv_mix) *)
