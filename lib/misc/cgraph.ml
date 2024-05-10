(* graphs as adjacency lists, values on both edges and vertices *)
type ('a, 'b) t = ('a, ('a, 'b Hashset.t) Hashtbl.t) Hashtbl.t

let print pp_node pp_edge fmt (graph : ('a, 'b) t) =
  Format.fprintf fmt "@[<v 2>[@,%a@]\n]"
    (Format.pp_print_seq (fun fmt (n, neighbors) ->
         Format.fprintf fmt "(%a, [%a])" pp_node n
           (Format.pp_print_seq
              ~pp_sep:(fun fmt () -> Format.fprintf fmt "; ")
              (fun fmt (n', e) ->
                Format.fprintf fmt "%a (%a)" pp_node n' (Hashset.print pp_edge)
                  e ) )
           (Hashtbl.to_seq neighbors) ) )
    (Hashtbl.to_seq graph)

(* deep copy *)
let copy (g : ('a, 'b) t) : ('a, 'b) t =
  let g' = Hashtbl.create (Hashtbl.length g) in
  Hashtbl.iter
    (fun v neighbours ->
      let neighbours' = Hashtbl.create (Hashtbl.length neighbours) in
      Hashtbl.iter
        (fun n edges -> Hashtbl.add neighbours' n (Hashset.copy edges))
        neighbours ;
      Hashtbl.add g' v neighbours' )
    g ;
  g'

(* adds "neighbour" to the list of neighbours of "node" if not already
   present *)
let add_neighbour (graph : ('a, 'b) t) (node : 'a) (neighbor : 'a) (edge : 'b) =
  match Hashtbl.find_opt graph node with
  | None ->
      let tbl = Hashtbl.create 1 in
      Hashtbl.add tbl neighbor (Hashset.singleton edge) ;
      Hashtbl.add graph node tbl
  | Some neighbors -> (
    match Hashtbl.find_opt neighbors neighbor with
    | Some edges -> Hashset.add edges edge
    | None -> Hashtbl.add neighbors neighbor (Hashset.singleton edge) )

let remove_neighbour (graph : ('a, 'b) t) node neighbor edge =
  match Hashtbl.find_opt graph node with
  | None -> ()
  | Some neighbors -> (
    match Hashtbl.find_opt neighbors neighbor with
    | None -> ()
    | Some edges -> Hashset.remove edges edge )

let add_edge (graph : ('a, 'b) t) (n1, n2) e : unit =
  add_neighbour graph n1 n2 e ;
  add_neighbour graph n2 n1 e

let remove_edge (graph : ('a, 'b) t) (n1, n2) e : unit =
  remove_neighbour graph n1 n2 e ;
  remove_neighbour graph n2 n1 e

(* iters on the list of edges for every pair in nodes *)
let iter_edges f nodes =
  let rec loop = function
    | [] | [_] -> ()
    | x :: xs ->
        List.iter (fun y -> f (x, y)) xs ;
        loop xs
  in
  loop nodes

(* adds a node if not already present, does nothing otherwise *)
let add_node (graph : ('a, 'b) t) n =
  match Hashtbl.find_opt graph n with
  | None -> Hashtbl.add graph n (Hashtbl.create 1)
  | _ -> ()

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
  | Some neighbours -> Hashtbl.iter (fun _ e -> Hashset.iter f e) neighbours

(* folder over the edges of a graph. If two or more edges hold the same value,
   only the first one may be folded on by setting ~duplicate to false *)
let fold_edges ?(duplicate = true) f acc (graph : ('a, 'b) t) =
  if duplicate then
    Hashtbl.fold (fun _e1 -> Hashtbl.fold (fun _e2 -> Hashset.fold f)) graph acc
  else
    let n = Hashtbl.length graph in
    let visited = Hashtbl.create n in
    Hashtbl.fold
      (fun _e1 ->
        Hashtbl.fold (fun _e2 ->
            Hashset.fold (fun e acc ->
                if Hashtbl.mem visited e then acc
                else (Hashtbl.add visited e true ; f e acc) ) ) )
      graph acc
