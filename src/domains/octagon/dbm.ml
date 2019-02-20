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

  (** Low level access to a cell of the DBM where `get m i j` returns DBM[i][j]. *)
  val get : t -> coord2D -> cell

  (** Monotonic write: we update the cell at (i,j) only if the value passed as argument is smaller than the one in the DBM. *)
  val set : t -> coord2D -> cell -> unit

  val empty : t

  val copy : t -> t

  (** The dimension of the DBM is its number of variables. *)
  val dimension: t -> int

  (** Extend the DBM with a new unbounded variable (immutable operation).
      The dimension is increased by 1.*)
  val extend_one : t -> t
end

module Make(B:Bound_sig.BOUND) = struct
  type cell = B.t
  type t = {
    dim: int;
    m: cell array;
  }

  let empty = {dim=0; m=[||]}

  let get dbm coord = dbm.m.(matpos2 coord)

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
end
