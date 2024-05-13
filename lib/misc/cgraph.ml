(* hypergraphs as a double representation: 1) a map from nodes to set of edges
   2) a map from edges to set of nodes *)

type ('a, 'b) t =
  {nodes: ('a, 'b Hashset.t) Hashtbl.t; edges: ('b, 'a Hashset.t) Hashtbl.t}

let nb_nodes g = Hashtbl.length g.nodes

let nb_edges g = Hashtbl.length g.edges

let get_edges g = Hashtbl.to_seq_keys g.edges |> List.of_seq

let get_nodes g = Hashtbl.to_seq_keys g.nodes |> List.of_seq

let print pp_node pp_edge fmt (graph : ('a, 'b) t) =
  Format.fprintf fmt "@[<v 2>@,%a@]"
    (Format.pp_print_seq (fun fmt (e, support) ->
         Format.fprintf fmt "%a, [%a]" pp_edge e (Hashset.print pp_node) support )
    )
    (Hashtbl.to_seq graph.edges)

(* deep copy *)
let copy (g : ('a, 'b) t) : ('a, 'b) t =
  let n' = Hashtbl.create (Hashtbl.length g.nodes) in
  let e' = Hashtbl.create (Hashtbl.length g.edges) in
  Hashtbl.iter (fun n edges -> Hashtbl.add n' n (Hashset.copy edges)) g.nodes ;
  Hashtbl.iter (fun e nodes -> Hashtbl.add e' e (Hashset.copy nodes)) g.edges ;
  {nodes= n'; edges= e'}

(* adds the edge 'r' to the set of edges of the node 'n'. It also adds n to the
   graph if not already present *)
let add_edge_to_node (graph : ('a, 'b) t) n e : unit =
  match Hashtbl.find_opt graph.nodes n with
  | None -> Hashtbl.add graph.nodes n (Hashset.singleton e)
  | Some edges -> Hashset.add edges e

(* adds the node 'n' to the set of nodes of the edge 'e'. It also adds 'e' to
   the graph if not already present *)
let add_node_to_edge (graph : ('a, 'b) t) e n : unit =
  match Hashtbl.find_opt graph.edges e with
  | None -> Hashtbl.add graph.edges e (Hashset.singleton n)
  | Some nodes -> Hashset.add nodes n

(* removes an edge from a graph, fails if does not exist *)
let remove_edge g e =
  match Hashtbl.find_opt g.edges e with
  | Some nodes ->
      Hashset.iter
        (fun n ->
          let set = Hashtbl.find g.nodes n in
          Hashset.remove set e )
        nodes ;
      Hashtbl.remove g.edges e
  | None -> invalid_arg "edge already removed"

(* returns a copy of g where the edge e is missing *)
let subset g e =
  let graph' = copy g in
  remove_edge graph' e ; graph'

(* builds a graph from a list of neighbouring relations *)
let build nb_node (supports : ('a list * 'b) list) : ('a, 'b) t =
  let nodes = Hashtbl.create nb_node in
  let edges = Hashtbl.create (List.length supports) in
  let graph = {nodes; edges} in
  List.iter
    (fun (ns, e) ->
      List.iter
        (fun n -> add_node_to_edge graph e n ; add_edge_to_node graph n e)
        ns )
    supports ;
  graph

(* iterates over the edges from a vertex *)
let iter_edges_from (f : 'b -> unit) (graph : ('a, 'b) t) (node : 'a) : unit =
  match Hashtbl.find_opt graph.nodes node with
  | None -> invalid_arg "Cgraph.iter_edges_from"
  | Some edges -> Hashset.iter f edges

(* iterates over the neighbours of a vertex *)
let iter_neighbours (f : 'a -> unit) (g : ('a, 'b) t) (n : 'a) =
  let visited = Hashtbl.create (Hashtbl.length g.nodes) in
  Hashtbl.add visited n true ;
  iter_edges_from
    (fun e ->
      Hashset.iter
        (fun n ->
          if not (Hashtbl.mem visited n) then (Hashtbl.add visited n true ; f n)
          )
        (Hashtbl.find g.edges e) )
    g n
