(* This module handles the obj format generation *)

type vertex = float * float * float
and face = (int * int * int)

(* printing *)
let print_vertex fmt (x,y,z) = Format.fprintf fmt "v %f %f %f\n" x y z

let print_face fmt (id1,id2,id3)  =
  Format.fprintf fmt "f %i %i %i\n" id1 id2 id3

(* vertex id generator *)
let gen_vert_id =
  let v_id = ref 0 in
  fun () ->
  incr v_id;
  !v_id

let square a b c d = (a,b,c),(a,c,d)

let print_vertex =
 (* cache to remove redundant vertices *)
  let h_v = Hashtbl.create 10000 in
  fun fmt v ->
  try Hashtbl.find h_v v
  with Not_found -> (
    let id = gen_vert_id() in
    Hashtbl.add h_v v id;
    Format.fprintf fmt "%a" print_vertex v;
    id
  )

let handle_c fmt acc (p0,p1,p2,p3,p4,p5,p6,p7) =
  let id0 = print_vertex fmt p0 in
  let id1 = print_vertex fmt p1 in
  let id2 = print_vertex fmt p2 in
  let id3 = print_vertex fmt p3 in
  let id4 = print_vertex fmt p4 in
  let id5 = print_vertex fmt p5 in
  let id6 = print_vertex fmt p6 in
  let id7 = print_vertex fmt p7 in
  let t1,t2 = square id0 id1 id2 id3
  and t3,t4 = square id4 id5 id6 id7
  and t5,t6 = square id0 id1 id5 id4
  and t7,t8 = square id2 id3 id7 id6
  and t9,t10 = square id0 id3 id7 id4
  and t11,t12 = square id1 id2 id6 id5 in
  t12::t11::t10::t9::t8::t7::t6::t5::t4::t3::t2::t1::acc

(* On the fly version of the previous utility *)
let print_to_file fmt cubelist =
  let faces = List.fold_left (handle_c fmt) [] cubelist in
  List.iter (print_face fmt) faces
