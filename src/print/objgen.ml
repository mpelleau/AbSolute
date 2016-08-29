(* This module handles the obj format generation *)

type point3d = float * float * float

let translate (x,y,z) (dx,dy,dz) = ((x+.dx), (y+.dy), (z+.dz))

let ( @> ) = translate

type obj = vertex Queue.t * face list
and vertex = point3d
and normal = point3d
and face = (int * int * int)

(* printing *)
let print_vertex fmt (x,y,z) = Format.fprintf fmt "v %f %f %f\n" x y z

let print_normal fmt (x,y,z) = Format.fprintf fmt "vn %f %f %f\n" x y z

let print_face fmt (id1,id2,id3)  =
  Format.fprintf fmt "f %i %i %i\n" id1 id2 id3

let print fmt ((vl,fl):obj) =
  Queue.iter (print_vertex fmt) vl;
  List.iter (print_face fmt) fl

(* an origin, width, height and depth *)
type cube = point3d * float * float * float

let of_triangle_list tris : obj =
  let v_id = ref 0 in let gen_vert_id () = incr v_id; !v_id in
  let queue = Queue.create () in
  (* removes useless vertices *)
  let h_v = Hashtbl.create 10000 in
  let id p =
    try Hashtbl.find h_v p
    with Not_found -> (
      let id = gen_vert_id() in
      Hashtbl.add h_v p id;
      Queue.add p queue; id
    )
  in
  (* removes useless faces *)
  let h_f = Hashtbl.create 1000 in
  let sort (a,b,c) =
    match List.sort compare [a;b;c] with [a;b;c] -> (a,b,c) | _ -> assert false
  in
  let rec aux acc t_list =
    match t_list with
    | [] -> acc
    | (p1,p2,p3)::tl ->
      let i1 = id p1 and i2 = id p2 and i3 = id p3 in
      let acc =
	if Hashtbl.mem h_f (i1,i2,i3) then acc
	else begin
	  let norm = sort (i1,i2,i3) in
	  Hashtbl.add h_f norm norm; norm::acc end
      in aux acc tl
  in queue,(aux [] tris)

(* assuming the points are sorted in direct order *)
let triangles_of_convex_face face =
  let rec aux acc pivot cur vertices =
    match vertices with
    | h::tl -> aux ((pivot,cur,h)::acc) pivot h tl
    | [] -> acc
  in
  match face with
  | a::b::c -> aux [] a b c
  | _ -> []

let build_cube_list (cubes: cube list) =
  let square a b c d = (a,b,c),(a,c,d) in
  let rec aux acc c_list =
    match c_list with
    | [] -> acc
    | (((x0,y0,z0) as p0),x,y,z)::tl ->
      let p0 = p0              and p1 = p0 @> (x,0.,0.)
      and p2 = p0 @> (x,y,0.)  and p3 = p0 @> (0.,y,0.) in
      let p4 = p0 @> (0.,0.,z) and p5 = p1 @> (0.,0.,z)
      and p6 = p2 @> (0.,0.,z) and p7 = p3 @> (0.,0.,z) in
      let t1,t2 = square p0 p1 p2 p3  and t3,t4 = square p4 p5 p6 p7
      and t5,t6 = square p0 p1 p5 p4  and t7,t8 = square p2 p3 p7 p6
      and t9,t10 = square p0 p3 p7 p4 and t11,t12 = square p1 p2 p6 p5 in
      let faces = t12::t11::t10::t9::t8::t7::t6::t5::t4::t3::t2::t1::acc in
      aux faces tl
  in (aux [] cubes) |> of_triangle_list

let print_to_file fn cubelist =
  let oc = open_out fn in
  let fmt = Format.formatter_of_out_channel oc in
  let o:obj = build_cube_list cubelist in
  Format.fprintf fmt "%a" print o

module Abs = Abstract_box.BoxF

let get_cube abs vars =
  let make_cube (a,b) (c,d) (e,f) = ((a,c,e), b-.a, d-.c, f-.e) in
  let open Abs in
  match vars with
  | None -> (
    match (Env.bindings abs) with
    | x::y::z::_ ->
      make_cube
	(Env.find (fst x) abs)
	(Env.find (fst y) abs)
	(Env.find (fst z) abs)
    | x::y::[] ->
      make_cube
	(Env.find (fst x) abs)
	(Env.find (fst y) abs)
	(1.,1.)
    | x::[] ->
      make_cube
	(Env.find (fst x) abs)
	(1.,1.)
	(1.,1.)
    | _ -> failwith "need at least 1 var")
  | _ -> failwith "niy"

let doit out values vars=
  List.rev_map (fun (e,_) -> get_cube e vars) values
  |> print_to_file out
