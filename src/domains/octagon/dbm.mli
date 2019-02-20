(* Documentation taken from the APRON project.

   We consider matrices of 2n*2n upper bounds.
   Let us denote by (i,j) the matrix element at line i, column j; the matrix
   induces the following constraints:
     Vj-Vi <= (2i,2j)
     Vj+Vi <= (2i+1,2j)
    -Vj-Vi <= (2i,2j+1)
    -Vj+Vi <= (2i+1,2j+1)

   Actually, this representation is redudant, and so, we manipulate
   2x2 block lower-triangular matrices.
   Only elements (i,j) such that j/2 <= i/2 are represented:

       j ->  0 1 2 3 4 5
            ___
        0  |_|_|
        1  |_|_|___
  i ->  2  |_|_|_|_|
        3  |_|_|_|_|___
        4  |_|_|_|_|_|_|
        5  |_|_|_|_|_|_|


                 j
             0     -2x0
            2x0      0
       i
           x0-x1  -x0-x1      0   -2x1
           x0+x1  -x0+x1     2x1    0

   Elements such that j/2 > i/2 are retreived by coherence: (i,j) = (j^1,i^1)
*)

(** 2D coordinate of an element in the DBM *)
type coord2D = int * int

(** position of (i,j) element, assuming j/2 <= i/2 *)
val matpos : coord2D -> int

(** position of (i,j) element, no assumption *)
val matpos2 : coord2D -> int

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

  (** Low-level representation of the DBM as a list. *)
  val to_list: t -> cell list
end

module Make(B:Bound_sig.BOUND) : DBM_sig with type cell = B.t
