(* This module handles the obj file format generation *)
type point3d = float * float * float
type cube = point3d * point3d * point3d * point3d
            * point3d * point3d * point3d * point3d
type id = int
type face = id * id * id

(* Vertex printing *)
let print_vertex fmt ((x,y,z):point3d) = Printf.fprintf fmt "v %f %f %f\n" x y z

(* Face printing *)
let print_face fmt ((id1,id2,id3):face)  =
  Printf.fprintf fmt "f %i %i %i\n" id1 id2 id3

(* Prints three faces at once *)
let print_face3 fmt (id1,id2,id3) (id1',id2',id3') (id1'',id2'',id3'')  =
  Printf.fprintf fmt "f %i %i %i\nf %i %i %i\nf %i %i %i\n"
    id1 id2 id3 id1' id2' id3' id1'' id2'' id3''

(* vertex id generator *)
let gen_vert_id : unit -> id =
  let v_id = ref 0 in
  fun () ->
  incr v_id;
  !v_id

(* prints a vertex and return the associated ID *)
let print_vertex : out_channel -> point3d -> id =
  (* cache to avoid the multiple printing of the same vertice *)
  let h_v = Hashtbl.create 10000000 in
  fun fmt v ->
  try Hashtbl.find h_v v
  with Not_found -> (
    let id = gen_vert_id() in
    Hashtbl.add h_v v id;
    Printf.fprintf fmt "%a" print_vertex v;
    id
  )

(* This function is expanded for efficiency purposes *)
(* It prints 8 points and then the faces between those points *)
let handle_c fmt (p0,p1,p2,p3,p4,p5,p6,p7) =
  let id0 = print_vertex fmt p0 in
  let id1 = print_vertex fmt p1 in
  let id2 = print_vertex fmt p2 in
  let id3 = print_vertex fmt p3 in
  let id4 = print_vertex fmt p4 in
  let id5 = print_vertex fmt p5 in
  let id6 = print_vertex fmt p6 in
  let id7 = print_vertex fmt p7 in
  let t1,t2 = (id0,id1,id2),(id0,id2,id3) in
  let t3,t4 = (id4,id5,id6),(id4,id6,id7) in
  let t5,t6 = (id0,id1,id5),(id0,id5,id4) in
  let t7,t8 = (id2,id3,id7),(id2,id7,id6) in
  let t9,t10 = (id0,id3,id7),(id0,id7,id4) in
  let t11,t12 = (id1,id2,id6),(id1,id6,id5) in
  print_face3 fmt t1 t2 t3;
  print_face3 fmt t4 t5 t6;
  print_face3 fmt t7 t8 t9;
  print_face3 fmt t10 t11 t12

(* Main function of the module. Prints the cube list *)
let print_to_file fmt (cubes: cube list) =
  List.iter (handle_c fmt) cubes
