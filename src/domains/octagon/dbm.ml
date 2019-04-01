type dbm_var = {
  l: int;
  c: int;
}
type 'b dbm_constraint = {
  v: dbm_var;
  d: 'b;
}
type dbm_interval = {
  lb: dbm_var;
  ub: dbm_var;
}

(* The test might be `c/2 >= l/2`, however we can avoid two divisions in the regular case by considering `c > l`.
   For example, in the valid case where c=3 and l=2, we still have the same result: c=2^1, l=3^1 . *)
let make_var l c = if c > l then {l=c lxor 1; c=l lxor 1} else {l=l; c=c}

let check_coherence v = if v.c/2 > v.l/2 then raise (Failure "variable must be initialized with `make_var` to be coherent.")

let inv v = (check_coherence v; { l=v.l lxor 1; c=v.c lxor 1 })
let is_lower_bound v = (check_coherence v; (v.l mod 2) = 0)
let is_upper_bound v = (check_coherence v; (v.l mod 2) = 1)
let as_interval v = if is_lower_bound v then {lb=v; ub=inv v} else {lb=inv v; ub=v}
let is_rotated v = (v.l / 2) <> (v.c / 2)

module type Fold_interval_sig =
sig
  val fold: ('a -> dbm_interval -> 'a) -> 'a -> int -> 'a
end

module Fold_intervals =
struct
  let fold f accu dimension =
    List.fold_left (fun accu l ->
      List.fold_left (fun accu c ->
        let accu = if l <> c then (* if rotated there are two intervals. *)
          f accu (as_interval {l=l*2; c=c*2})
        else
          accu in
        f accu (as_interval {l=l*2; c=c*2+1})
      ) accu (Tools.range 0 l)
    ) accu (Tools.range 0 (dimension-1))
end

module Fold_intervals_canonical =
struct
  let fold f accu dimension =
    List.fold_left (fun accu k ->
      f accu (as_interval {l=k*2; c=k*2+1})
    ) accu (Tools.range 0 (dimension-1))
end

module Fold_intervals_rotated =
struct
  let fold f accu dimension =
    List.fold_left (fun accu l ->
      List.fold_left (fun accu c ->
        let accu = f accu (as_interval {l=l*2; c=c*2}) in
        f accu (as_interval {l=l*2; c=c*2+1})
      ) accu (Tools.range 0 (l-1))
    ) accu (Tools.range 1 (dimension-1))
end

module type DBM_sig =
sig
  module B: Bound_sig.BOUND
  type bound = B.t
  type t
  val init: int -> t
  val get : t -> dbm_var -> bound
  val set : t -> bound dbm_constraint -> t
  val project: t -> dbm_interval -> (bound * bound)
  val copy : t -> t
  val dimension: t -> int
  val to_list: t -> bound list
  val print: Format.formatter -> t -> unit
end

(* module Make(B:Bound_sig.BOUND) = struct
  module B=B
  type bound = B.t
  type t = {
    dim: int;
    m: bound array;
  }

  let init n =
    let rec size n = if n = 0 then 0 else (n*2*2) + size (n-1) in
    {dim=n; m=Array.make (size n) B.inf}

  (* Precondition: `v` is coherent, i.e. v.x/2 <= v.y/2 *)
  let matpos v = (check_coherence v; v.c + ((v.l+1)*(v.l+1))/2)

  let get dbm v = dbm.m.(matpos v)

  let set dbm dbm_cons =
    let pos = matpos dbm_cons.v in
    (if B.gt dbm.m.(pos) dbm_cons.d then dbm.m.(pos) <- dbm_cons.d;
    dbm)

  let project dbm itv = (B.neg (get dbm itv.lb)), get dbm itv.ub
  let copy dbm = {dim=dbm.dim; m=Array.copy dbm.m}

  let dimension dbm = dbm.dim

  let to_list dbm = Array.to_list dbm.m

  let print fmt dbm =
    for l = 0 to (2*dbm.dim-1) do
      for c = 0 to (2*dbm.dim-1) do
        Format.fprintf fmt "%s " (B.to_string (get dbm (make_var l c)))
      done;
      Format.fprintf fmt "\n"
    done
end
 *)
module Make(B:Bound_sig.BOUND) = struct
  module B=B
  type bound = B.t
  type t = {
    dim: int;
    m: bound Parray.t;
  }

  let init n =
    let rec size n = if n = 0 then 0 else (n*2*2) + size (n-1) in
    {dim=n; m=Parray.make (size n) B.inf}

  (* Precondition: `v` is coherent, i.e. v.x/2 <= v.y/2 *)
  let matpos v = (check_coherence v; v.c + ((v.l+1)*(v.l+1))/2)

  let get dbm v = Parray.get dbm.m (matpos v)

  let set dbm dbm_cons =
    let pos = matpos dbm_cons.v in
    if B.gt (Parray.get dbm.m pos) dbm_cons.d then
      {dbm with m=Parray.set dbm.m pos dbm_cons.d}
    else
      dbm

  let project dbm itv = (B.neg (get dbm itv.lb)), get dbm itv.ub
  let copy dbm = dbm

  let dimension dbm = dbm.dim

  let to_list dbm = Parray.to_list dbm.m

  let print fmt dbm =
    for l = 0 to (2*dbm.dim-1) do
      for c = 0 to (2*dbm.dim-1) do
        Format.fprintf fmt "%s " (B.to_string (get dbm (make_var l c)))
      done;
      Format.fprintf fmt "\n"
    done
end
