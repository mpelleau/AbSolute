open Tools
open Signature

(* lifts a numeric domain to a boolean one *)
module type B = sig
  module Make : functor (N : Numeric) -> Domain
end

(* domain combinators of arity 1 *)
module type D1 = sig
  module Make : functor (D : Domain) -> Domain
end

(* domain combinators of arity 2 *)
module type D2 = sig
  module Make : functor (A : Domain) (B : Domain) -> Domain
end

(* boolean domain maps *)
let booleans = ref VarMap.empty

(* numeric domain maps *)
let numeric = ref VarMap.empty

(* combinators *)
let arity_1 = ref VarMap.empty

let arity_2 = ref VarMap.empty

(** collect all domain names *)
let get_all =
  let aux m = List.map fst @@ VarMap.bindings m in
  fun () -> aux !numeric @ aux !arity_1 @ aux !arity_2

(* map updating *)
let update name d = function
  | None -> Some d
  | Some _ -> failwith ("name clash between abstract domains: " ^ name)

(** registers a domain into the list of available abstract domains *)
let register_numeric name (d : (module Numeric)) =
  numeric := VarMap.update name (update name d) !numeric

(* registers a domain into the list of available abstract domains *)
let register_boolean name (d : (module B)) =
  booleans := VarMap.update name (update name d) !booleans

(** registers a domain combinator into the list of combinators of arity 1 *)
let register1 name (d : (module D1)) =
  arity_1 := VarMap.update name (update name d) !arity_1

(** registers a domain combinator into the list of combinators of arity 2 *)
let register2 name (d : (module D2)) =
  arity_2 := VarMap.update name (update name d) !arity_2

(* splits a string into a list of args. ignores the nested calls *)
let split_args (s : string) =
  let l = String.length s in
  let rec loop acc first i nested =
    if i = l then
      let arg = String.sub s first (i - first) in
      List.rev (arg :: acc)
    else
      match s.[i] with
      | '(' -> loop acc first (i + 1) (nested + 1)
      | ')' -> loop acc first (i + 1) (nested - 1)
      | ',' ->
          if nested = 0 then
            let arg = String.sub s first (i - first) in
            loop (arg :: acc) (i + 1) (i + 1) nested
          else loop acc first (i + 1) nested
      | _ -> loop acc first (i + 1) nested
  in
  loop [] 0 0 0

let parse name boolean =
  let rec loop name : (module Domain) =
    match String.index_opt name '(' with
    | None ->
        let (module M) = VarMap.find name !numeric in
        let (module B) = VarMap.find boolean !booleans in
        (module B.Make (M))
    | Some 0 -> failwith "a domain description can not begin with a parenthesis"
    | Some i -> (
      match String.rindex_opt name ')' with
      | None -> failwith "unmatched parenthesis '('"
      | Some j -> (
          let s = String.sub name 0 i in
          let args = split_args (String.sub name (i + 1) (j - i - 1)) in
          match args with
          | [arg] ->
              let (module Mk) = VarMap.find s !arity_1 in
              let (module Arg : Domain) = loop arg in
              (module Mk.Make (Arg))
          | [arg1; arg2] ->
              let (module Mk) = VarMap.find s !arity_2 in
              let (module Arg1 : Domain) = loop arg1 in
              let (module Arg2 : Domain) = loop arg2 in
              (module Mk.Make (Arg1) (Arg2))
          | _ -> failwith "max arity 2 for domain description" ) )
  in
  try loop name
  with Not_found ->
    fail_fmt "domain unknown %s. Possible domains are %a" name
      Format.(
        pp_print_list ~pp_sep:(fun f () -> fprintf f ", ") pp_print_string)
      (get_all ())

(* Registering the abstract domains *)
let () =
  (* numeric *)
  register_numeric "box" (module Cartesian.BoxF) ;
  register_numeric "boxS" (module Cartesian.BoxStrict) ;
  register_numeric "apronbox" (module ADCP.BoxCP) ;
  register_numeric "poly" (module ADCP.PolyCP) ;
  register_numeric "oct" (module ADCP.OctCP) ;
  (* boolean *)
  register_boolean "boolean" (module Boolean) ;
  register_boolean "uniontree" (module Uniontree) ;
  (* combinators *)
  register2 "product" (module Product)
