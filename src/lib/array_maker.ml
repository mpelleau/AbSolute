(* This file provides a generic way to build high-level module on *)
(* array like structures                                          *)

module type ArrayLike = sig

  type elem
  type t

  val get : t -> int -> elem
  val set :  t -> int -> elem -> unit
  val length : t -> int
  val make : elem -> int -> t
  val empty : t

end

module Make (A:ArrayLike) = struct

  let array_fold f a arr =
    let size = A.length arr in
    let rec aux accu idx =
      if idx >= size then accu
      else aux (f accu (A.get arr idx)) (idx+1)
    in aux a 0

  let array_iter f arr =
    array_fold (fun _ a -> f a) () arr

  let array_for_all pred arr =
    try
      array_iter (fun a -> if pred a |> not then raise Exit) arr;
      true
    with Exit -> false

  let array_to_list arr =
    let rec aux accu idx =
      if idx <= 0 then accu
      else aux ((A.get arr idx)::accu) (idx-1)
    in aux [] (A.length arr - 1)

  let array_of_list = function
    | [] -> A.empty
    | h::tl as l ->
       let size = List.length tl in
       let ear = A.make h size in
       List.iteri (fun i b -> A.set ear i b) l;
       ear
end

module LinconsExt = Make (struct
  open Apron

  open Lincons1

  type elem = t
  type t = earray

  let get = array_get

  let set = array_set

  let length = array_length

  let make elem = array_make elem.env

  let empty = array_make (Environment.make [||] [||]) 0
end)

module TconsExt = Make (struct
  open Apron

  open Tcons1

  type elem = t
  type t = earray

  let get = array_get

  let set = array_set

  let length = array_length

  let make elem = array_make elem.env

  let empty = array_make (Environment.make [||] [||]) 0
end)
