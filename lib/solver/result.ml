(** This module defines solution of the abstract solver as covers *)

(** The type of cover where sure is an under-approximation of the solution set
    and the union of sure and unsure is an over-approximaton of the solution set *)
type 'a t =
  { sure: 'a list  (** elements that satisfy the constraints *)
  ; unsure: 'a list  (** elements that MAY satisfy the constraints *)
  ; nb_sure: int  (** size of sure list *)
  ; nb_unsure: int  (** size of unsure list *)
  ; vol_sure: float  (** volume of the elements in the sure list *)
  ; vol_unsure: float  (** volume of the elements in the unsure list *)
  ; nb_steps: int  (** number of steps of the solving process *)
  ; best_value: Q.t  (** best value found during the optimization *) }

(** empty result *)
let empty =
  { sure= []
  ; unsure= []
  ; nb_sure= 0
  ; nb_unsure= 0
  ; vol_sure= 0.
  ; vol_unsure= 0.
  ; nb_steps= 0
  ; best_value= Q.zero }

(** computes the inner ratio (between 0 and 1) of a solution *)
let inner_ratio res =
  let divider = res.vol_sure +. res.vol_unsure in
  if divider = 0. then 0. else res.vol_sure /. divider

(** adds an inner element to a result *)
let add_inner res x = {res with sure= x :: res.sure; nb_sure= res.nb_sure + 1}

(** adds an outer element to a result *)
let add_outer res x =
  {res with unsure= x :: res.unsure; nb_unsure= res.nb_unsure + 1}

(** printer *)
let print fmt res =
  Format.fprintf fmt
    "\n\
     #inner boxes: %d\n\
     #boundary boxes: %d\n\
     #created nodes: %d\n\n\
     inner volume = %f\n\
     boundary volume = %f\n\
     total volume = %f%!\n"
    res.nb_sure res.nb_unsure res.nb_steps res.vol_sure res.vol_unsure
    (res.vol_sure +. res.vol_unsure)
