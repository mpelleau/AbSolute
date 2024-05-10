(* hacky-ish implementation of hashset using hashtbl from keys to unit *)

type 'a t = ('a, unit) Hashtbl.t

let print pp_elem fmt (h : 'a t) =
  (Format.pp_print_seq (fun fmt (e, _) -> pp_elem fmt e))
    fmt (h |> Hashtbl.to_seq)

let mem : 'a t -> 'a -> bool = Hashtbl.mem

let singleton x : 'a t =
  let res = Hashtbl.create 1 in
  Hashtbl.add res x () ; res

let add (h : 'a t) x = Hashtbl.replace h x ()

let remove : 'a t -> 'a -> unit = Hashtbl.remove

let iter f (h : 'a t) = Hashtbl.iter (fun a () -> f a) h

let fold f (h : 'a t) = Hashtbl.fold (fun a _ -> f a) h

let copy : 'a t -> 'a t = Hashtbl.copy
