type dbm_var = {
  x: int;
  y: int;
}
type 'b dbm_constraint = {
  v: dbm_var;
  c: 'b;
}
type dbm_interval = {
  lb: dbm_var;
  ub: dbm_var;
}

let inv v = { x=v.x lxor 1; y=v.y lxor 1 }
let is_lower_bound v = (v.x mod 2) = 0
let is_upper_bound v = (v.x mod 2) = 1
let as_interval v = if is_lower_bound v then {lb=v; ub=inv v} else {lb=inv v; ub=v}

module type Fold_interval_sig =
sig
  val fold: ('a -> dbm_interval -> 'a) -> 'a -> int -> 'a
end

module Fold_intervals =
struct
  let fold f accu dimension =
    List.fold_left (fun accu x ->
      List.fold_left (fun accu y ->
        let accu = f accu (as_interval {x=x*2; y=y*2+1}) in
        if x <> y then (* if rotated there are two intervals. *)
          f accu (as_interval {x=x*2; y=y*2})
        else
          accu
      ) accu (Tools.range 0 x)
    ) accu (Tools.range 0 (dimension-1))
end

module Fold_intervals_canonical =
struct
  let fold f accu dimension =
    List.fold_left (fun accu k ->
      f accu (as_interval {x=k*2; y=k*2+1})
    ) accu (Tools.range 0 (dimension-1))
end

module Fold_intervals_rotated =
struct
  let fold f accu dimension =
    List.fold_left (fun accu x ->
      List.fold_left (fun accu y ->
        let accu = f accu (as_interval {x=x*2; y=y*2+1}) in
        f accu (as_interval {x=x*2; y=y*2})
      ) accu (Tools.range 0 (x-1))
    ) accu (Tools.range 1 (dimension-1))
end

module type DBM_sig =
sig
  module B: Bound_sig.BOUND
  type bound = B.t
  type t
  val init: int -> t
  val get : t -> dbm_var -> bound
  val get_coherent : t -> dbm_var -> bound
  val set : t -> bound dbm_constraint -> unit
  val set_coherent : t -> bound dbm_constraint -> unit
  val project: t -> dbm_interval -> (bound * bound)
  val copy : t -> t
  val dimension: t -> int
  val to_list: t -> bound list
  val print: Format.formatter -> t -> unit
end

module Make(B:Bound_sig.BOUND) = struct
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
  let matpos v = v.y + ((v.x+1)*(v.x+1))/2
  let matpos2 v =
    if v.y > v.x then matpos {x=(v.y lxor 1); y=(v.x lxor 1)}
    else matpos v

  let get dbm v = dbm.m.(matpos2 v)
  let get_coherent dbm v = dbm.m.(matpos v)
  let set_pos dbm pos c = if B.gt dbm.m.(pos) c then dbm.m.(pos) <- c
  let set dbm dbm_cons = set_pos dbm (matpos2 dbm_cons.v) dbm_cons.c
  let set_coherent dbm dbm_cons = set_pos dbm (matpos dbm_cons.v) dbm_cons.c
  let project dbm itv = (B.neg (get_coherent dbm itv.lb)), get_coherent dbm itv.ub
  let copy dbm = {dim=dbm.dim; m=Array.copy dbm.m}

  let dimension dbm = dbm.dim

  let to_list dbm = Array.to_list dbm.m

  let print fmt dbm =
    for i = 0 to (2*dbm.dim-1) do
      for j = 0 to (2*dbm.dim-1) do
        Format.fprintf fmt "%s " (B.to_string (get dbm {x=i;y=j}))
      done;
      Format.fprintf fmt "\n"
    done
end
