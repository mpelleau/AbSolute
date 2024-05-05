(* graphs as adjacency lists, values on both edges and vertices *)
type ('a, 'b) t = ('a, ('a, 'b) Hashtbl.t) Hashtbl.t

let print print_node print_edge fmt (graph : ('a, 'b) t) =
  Format.fprintf fmt "@[<v 2>[@,%a@]\n]"
    (Format.pp_print_list (fun fmt (node, neighbors) ->
         Format.fprintf fmt "(%a, [%a])" print_node node
           (Format.pp_print_list
              ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
              (fun fmt (a, b) ->
                Format.fprintf fmt "%a (%a)" print_node a print_edge b ) )
           (List.of_seq (Hashtbl.to_seq neighbors)) ) )
    (List.of_seq (Hashtbl.to_seq graph))

(* adds "neighbour" to the list of neighbours of "node" if not already
   present *)
let add_neighbour (graph : ('a, 'b) t) node neighbor edge =
  match Hashtbl.find_opt graph node with
  | None ->
      let tbl = Hashtbl.create 1 in
      Hashtbl.add tbl neighbor edge ;
      Hashtbl.add graph node tbl
  | Some neighbors -> Hashtbl.add neighbors neighbor edge

(* adds a node if not already present, does nothing otherwise *)
let add_node (graph : ('a, 'b) t) n =
  match Hashtbl.find_opt graph n with
  | None -> Hashtbl.add graph n (Hashtbl.create 1)
  | _ -> ()

let add_edge (graph : ('a, 'b) t) (n1, n2) e : unit =
  add_neighbour graph n1 n2 e ;
  add_neighbour graph n2 n1 e

let remove_edges (_graph : ('a, 'b) t) _e : unit = failwith "graph remove edge"

(* iters on the list of edges for every pair in nodes *)
let iter_edges f nodes =
  let rec loop = function
    | [] | [_] -> ()
    | x :: xs ->
        List.iter (fun y -> f (x, y)) xs ;
        loop xs
  in
  loop nodes

(* builds a graph from a list of neighbouring relations *)
let build size (neighbours : ('a list * 'b) list) : ('a, 'b) t =
  let g = Hashtbl.create size in
  List.iter
    (function
      | [], _ -> ()
      | [one], _ -> add_node g one
      | ns, id -> iter_edges (fun e -> add_edge g e id) ns )
    neighbours ;
  g

(* iterates over the edges from a vertex *)
let iter_edges_from (f : 'b -> unit) (graph : ('a, 'b) t) (node : 'a) : unit =
  match Hashtbl.find_opt graph node with
  | None -> invalid_arg "Egraph.iter_edges_from"
  | Some neighbours -> Hashtbl.iter (fun _ e -> f e) neighbours

(* folder over the edges of a graph. If two or more edges hold the same value,
   the second may be ignored by setting ~duplicate to false *)
let fold_edges ?(duplicate = true) f acc (graph : ('a, 'b) t) =
  if duplicate then
    Hashtbl.fold (fun _e1 -> Hashtbl.fold (fun _e2 -> f)) graph acc
  else
    let n = Hashtbl.length graph in
    let visited = Hashtbl.create n in
    Hashtbl.fold
      (fun _e1 ->
        Hashtbl.fold (fun _e2 c acc ->
            if Hashtbl.mem visited c then acc
            else (Hashtbl.add visited c true ; f c acc) ) )
      graph acc
