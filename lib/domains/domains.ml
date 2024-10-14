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

(* product combinators *)
module type D2 = sig
  module Make : functor (D1 : Domain) (D2 : Domain) -> Domain
end

(* boolean domain maps *)
let booleans = ref VarMap.empty

(* numeric domain maps *)
let numerics = ref VarMap.empty

(* combinators *)
let arity_1 = ref VarMap.empty

let arity_2 = ref VarMap.empty

(* full domains maps *)
let domains = ref VarMap.empty

(** collect all domain names *)
let get_all =
  let aux m = List.map fst @@ VarMap.bindings m in
  fun () -> aux !numerics @ aux !arity_1 @ aux !arity_2

(* map updating *)
let update name d = function
  | None -> Some d
  | Some _ -> failwith ("name clash between abstract domains: " ^ name)

(** registers a domain into the list of available abstract domains *)
let register_numeric name (d : (module Numeric)) =
  numerics := VarMap.update name (update name d) !numerics

(* registers a domain into the list of available abstract domains *)
let register_boolean name (d : (module B)) =
  booleans := VarMap.update name (update name d) !booleans

let register_1 name (d : (module D1)) =
  arity_1 := VarMap.update name (update name d) !arity_1

let register_2 name (d : (module D2)) =
  arity_2 := VarMap.update name (update name d) !arity_2

(* builds the name of domain as it will be stored in domain map *)
let domain_name num boolean = num ^ "," ^ boolean

let register num boolean (d : (module Domain)) =
  let name = domain_name num boolean in
  domains := VarMap.update name (update name d) !domains

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

(* checks if the domain name is already known, and if not builds the generic
   representation *)
let parse (num : string) (bool : string) : (module Domain) =
  let rec loop num : (module Domain) =
    match String.index_opt num '(' with
    | None ->
        let (module M) = VarMap.find num !numerics in
        let (module B) = VarMap.find bool !booleans in
        (module B.Make (M))
    | Some 0 -> failwith "a domain description can not begin with a parenthesis"
    | Some i -> (
      match String.rindex_opt num ')' with
      | None -> failwith "unmatched parenthesis '('"
      | Some j -> (
          let s = String.sub num 0 i in
          let args = split_args (String.sub num (i + 1) (j - i - 1)) in
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
  (* check if domain is already known, otherwise tries to build it *)
  match VarMap.find_opt (domain_name num bool) !domains with
  | Some (module D) -> (module D)
  | None -> (
    try loop num
    with Not_found ->
      fail_fmt "domain unknown %s. Possible domains are %a" num
        Format.(
          pp_print_list ~pp_sep:(fun f () -> fprintf f ", ") pp_print_string )
        (get_all ()) )

let iterator () : (module Propagator) =
  match !Constant.iterator with
  | "graph" -> (module Graphiterator)
  | "roundrobin" -> (module Iterator)
  | s -> fail_fmt "Domains.iterator: unknown propagator %s" s

module BoxF : Numeric = Cartesian.BoxF

module BoxS : Numeric = Cartesian.BoxStrict

module ApronBox : Numeric = Relational.Box

module Poly : Numeric = Relational.Poly

module Oct : Numeric = Relational.Oct

module Alias : Numeric = Alias

module BoxSXAlias = Product.BoxSXAlias

module Product : D2 = Product

module Boolean : B = Boolean

module Utree : B = Uniontree

(* Registering the abstract domains *)
let () =
  (* numerics *)
  register_numeric "box" (module BoxF) ;
  register_numeric "boxS" (module BoxS) ;
  register_numeric "apronbox" (module ApronBox) ;
  register_numeric "poly" (module Poly) ;
  register_numeric "oct" (module Oct) ;
  register_numeric "alias" (module Alias) ;
  (* booleans *)
  register_boolean "boolean" (module Boolean) ;
  register_boolean "uniontree" (module Uniontree) ;
  (* combinators *)
  register_2 "product" (module Product) ;
  register "product(box,alias)" "boolean" (module BoxSXAlias)
