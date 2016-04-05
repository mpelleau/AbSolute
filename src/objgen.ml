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

let build_cube_list (cubes: cube list) = 
  let v_id = ref 0 in
  let gen_vert_id () = incr v_id; !v_id in
  let queue = Queue.create () in
  let tri a b c = (a,b,c) in
  let square a b c d = (tri a b c),(tri a c d) in
  let rec aux acc c_list = 
    let add p = Queue.add p queue; gen_vert_id() in
    match c_list with
    | [] -> acc
    | (((x0,y0,z0) as p0),x,y,z)::tl ->
      let p1 = p0 @> (x,0.,0.)
      and p2 = p0 @> (x,y,0.)
      and p3 = p0 @> (0.,y,0.) in
      let up = (0.,0.,z) in
      let p4 = p0 @> up
      and p5 = p1 @> up
      and p6 = p2 @> up
      and p7 = p3 @> up in
      let i0 = add p0 and i1 = add p1 and i2 = add p2 and i3 = add p3
      and i4 = add p4 and i5 = add p5 and i6 = add p6 and i7 = add p7 in
      let f1,f2 = square i0 i1 i2 i3
      and f3,f4 = square i4 i5 i6 i7
      and f5,f6 = square i0 i1 i5 i4
      and f7,f8 = square i2 i3 i7 i6
      and f9,f10 = square i0 i3 i7 i4
      and f11,f12 = square i1 i2 i6 i5 in
      let faces = f12::f11::f10::f9::f8::f7::f6::f5::f4::f3::f2::f1::acc in
      aux faces tl
  in queue,(aux [] cubes)

let print_to_file fn cubelist = 
  let oc = open_out fn in
  let fmt = Format.formatter_of_out_channel oc in
  let o:obj = build_cube_list cubelist in
  Format.fprintf fmt "%a" print o

module Abs = Abstract_box.BoxF

let consistency abs tab =
  let abs' = List.fold_left Abs.meet abs tab in
  (if Abs.is_bottom abs' then `Empty 
   else if List.for_all (Abs.sat_cons abs') tab then `Full
   else `Maybe)
    ,abs'
      
let draw abs info col vars =
  if !Constant.visualization then
    Vue.draw (Abs.points_to_draw abs vars) col info
      
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

let explore_3d abs constrs vars_3d =
  let nb_sol = ref 0 and nb_steps = ref 1 in 
  let cubes : cube list ref = ref [] in
  let add abs = cubes:=(get_cube abs vars_3d)::!cubes in
  let queue = Queue.create () in
  Queue.add abs queue;
  while Queue.is_empty queue |> not do
    let cons,abs' = consistency (Queue.take queue) constrs in
    match cons with
    | `Empty -> ()
    | `Full -> add abs'; incr nb_sol
    | `Maybe  ->
      (match (Abs.is_small abs' !Constant.precision) with
      | true,_ -> add abs'; incr nb_sol
      | _,exprs when !nb_sol < !Constant.max_sol ->
        Abs.split abs' exprs |> List.iter (fun e -> incr nb_steps; Queue.add e queue)
      | _ -> add abs'
      )
  done;
  !cubes

let solve_3d prob out = 
  let open Syntax in
  let abs = Abs.of_problem prob in
  let cubes = explore_3d abs prob.constraints prob.to_draw in
  print_to_file out cubes
