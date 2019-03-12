type coord2D = int * int
let matpos (i,j) = j + ((i+1)*(i+1))/2
let matpos2 (i,j) =
  (* Coherence *)
  if j > i then matpos ((j lxor 1),(i lxor 1))
  else matpos (i,j)

module type DBM_sig =
sig
  type cell
  type t
  type dbm_constraint = coord2D * cell
  val get : t -> coord2D -> cell
  val get' : t -> coord2D -> cell
  val set : t -> coord2D -> cell -> unit
  val empty : t
  val copy : t -> t
  val dimension: t -> int
  val extend_one : t -> t
  val to_list: t -> cell list
  val print: Format.formatter -> t -> unit
end

module Make(B:Bound_sig.BOUND) = struct
  type cell = B.t
  type t = {
    dim: int;
    m: cell array;
  }
  type dbm_constraint = coord2D * cell

  let empty = {dim=0; m=[||]}

  let get dbm coord = dbm.m.(matpos2 coord)
  let get' dbm coord = dbm.m.(matpos coord)

  let set dbm coord v =
    let pos = matpos2 coord in
    if B.gt dbm.m.(pos) v then
      dbm.m.(pos) <- v

  let copy dbm = {dim=dbm.dim; m=Array.copy dbm.m}

  let dimension dbm = dbm.dim

  let extend_one dbm =
    let dbm = copy dbm in
    let dim = dbm.dim + 1 in
    let top = B.inf in
    let row = Array.make (dim*2*2) top in
    let m = Array.append dbm.m row in
    {dim=dim; m=m}

  let to_list dbm = Array.to_list dbm.m

  let print fmt dbm =
    for i = 0 to (2*dbm.dim-1) do
      for j = 0 to (2*dbm.dim-1) do
        Format.fprintf fmt "%s " (B.to_string (get dbm (i,j)))
      done;
      Format.fprintf fmt "\n"
    done
end
