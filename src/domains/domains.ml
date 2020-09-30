open Tools
open Signature

type dom_list = (module Domain) list

module type MK1 = sig module Make : functor (D:Domain) -> Domain end

module type MK2 = sig
  module Make : functor(A:Domain) -> functor(B:Domain) -> Domain
end

(* module maps *)
let arity_0 = ref VarMap.empty
let arity_1 = ref VarMap.empty
let arity_2 = ref VarMap.empty

let get_all =
  let aux m = List.map fst @@ VarMap.bindings m in
  fun () ->
  (aux !arity_0)@(aux !arity_1)@(aux !arity_2)

(* map updating function *)
let update name d = function
  | None -> Some d
  | Some _ -> failwith ("name clash between abstract domains: "^name)

(** registers an abstract domain into the list of available abstract
   domain *)
let register0 name (d:(module Domain)) =
  arity_0 := VarMap.update name (update name d) !arity_0

(** registers an abstract domain combinator into the list of
   combinators of arity 1 *)
let register1 name (d:(module MK1)) =
  arity_1 := VarMap.update name (update name d) !arity_1

(** registers an abstract domain combinator into the list of
   combinators of arity 2 *)
let register2 name (d:(module MK2)) =
  arity_2 := VarMap.update name (update name d) !arity_2

(** splits a string into a list of args. ignores the nested calls *)
let split_args (s:string) =
  let l = String.length s in
  let rec loop acc first i nested =
    if i = l then
      let arg = String.sub s first (i-first) in
      List.rev (arg::acc)
    else
      match s.[i] with
      | '(' -> loop acc first (i+1) (nested+1)
      | ')' -> loop acc first (i+1) (nested-1)
      | ',' -> if nested = 0 then
                 let arg = String.sub s first (i-first) in
                 loop (arg::acc) (i+1) (i+1) nested
               else loop acc first (i+1) nested
      | _ -> loop acc first (i+1) nested
  in loop [] 0 0 0

(** builds the abstract domain corresponding to the name given in
   parameter *)
let rec parse name : (module Domain) =
  match String.index_opt name '(' with
  | None -> let (module M) = VarMap.find_fail name !arity_0 in
            (module M)
  | Some 0 -> failwith "a domain description can not begin with a parenthesis"
  | Some i ->
     (match String.rindex_opt name ')' with
      | None -> failwith "unmatched parenthesis '('"
      | Some j ->
         let s = String.sub name 0 i in
         let args = split_args (String.sub name (i+1) (j-i-1)) in
         (match args with
          | [arg] ->
             let (module Mk) = VarMap.find_fail s !arity_1 in
             let (module Arg:Domain) = parse arg in
             (module Mk.Make(Arg))
          | [arg1; arg2] ->
             let (module Mk) = VarMap.find_fail s !arity_2 in
             let (module Arg1:Domain) = parse arg1 in
             let (module Arg2:Domain) = parse arg2 in
             (module Mk.Make(Arg1)(Arg2))
          | _ -> failwith "max arity 2 for domain description"))
  | exception Not_found ->
     fail_fmt "domain unknown %s. Possible domains are %a"
       name (Format.(pp_print_list ~pp_sep:(fun f () -> fprintf f ", ")
               (fun f -> fprintf f "%s"))) (get_all())

(* Registering the abstract domains *)

let _ =
  register0 "box"  (module Boolean.Make(Abstract_box.BoxF));
  register0 "boxS" (module Boolean.Make(Abstract_box.BoxStrict));
  register0 "apronbox" (module Boolean.Make(ADCP.BoxCP));
  register0 "poly" (module Boolean.Make(ADCP.PolyCP));
  register0 "oct" (module Boolean.Make(ADCP.OctCP));
  register2 "product" (module Product);
