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

let connected_components (graph : ('a, 'b) t) : ('a, 'b) t list =
  (* adds to the current connected component the neighbours of 'node' that are
     not already visited *)
  let n = Hashtbl.length graph in
  let visited = Hashtbl.create n in
  let rec explore (graph : ('a, 'b) t) component node =
    if not (Hashtbl.mem visited node) then (
      Hashtbl.add visited node true ;
      let neighbours = Hashtbl.find graph node in
      Hashtbl.add component node neighbours ;
      Hashtbl.iter (fun n _e -> explore graph component n) neighbours )
  in
  let components = ref [] in
  Hashtbl.iter
    (fun node _e ->
      if not (Hashtbl.mem visited node) then (
        let comp = Hashtbl.create 1 in
        explore graph comp node ;
        components := comp :: !components ) )
    graph ;
  !components

(* Tarjan's algorithm for bridge finding *)
let find_bridges (graph : ('a, 'b) t) =
  let n = Hashtbl.length graph in
  let visited = Hashtbl.create n in
  let disc = Hashtbl.create n in
  let low = Hashtbl.create n in
  let parent = Hashtbl.create n in
  let time = ref 0 in
  let result = ref [] in
  let rec dfs u =
    Hashtbl.add visited u true ;
    Hashtbl.add disc u !time ;
    Hashtbl.add low u !time ;
    incr time ;
    Hashtbl.iter
      (fun v _ ->
        if not (Hashtbl.mem visited v) then (
          Hashtbl.add parent v u ;
          dfs v ;
          let low_u = Hashtbl.find low u in
          let low_v = Hashtbl.find low v in
          let disc_u = Hashtbl.find disc u in
          Hashtbl.add low u (min low_u low_v) ;
          if low_v > disc_u then result := (u, v) :: !result )
        else if (not (Hashtbl.mem parent u)) || Hashtbl.find parent u <> v then
          let low_u = Hashtbl.find low u in
          let disc_v = Hashtbl.find disc v in
          Hashtbl.add low u (min low_u disc_v) )
      ( match Hashtbl.find_opt graph u with
      | None ->
          Format.printf "Graph.find_bridges: node not found\n" ;
          raise Not_found
      | Some succs -> succs )
  in
  Hashtbl.iter (fun n _ -> if not (Hashtbl.mem visited n) then dfs n) graph ;
  !result

(* finds the best bridge among a list of bridges *)
let central_bridge (graph : ('a, 'b) t) (bridges : ('a * 'a) list) =
  let size = Hashtbl.length graph in
  let score (v1, v2) =
    let score = ref 0 in
    let visited = Hashtbl.create size in
    let rec dfs u =
      incr score ;
      Hashtbl.add visited u true ;
      Hashtbl.iter
        (fun v _ -> if (not (Hashtbl.mem visited v)) && not (v = v2) then dfs v)
        ( match Hashtbl.find_opt graph u with
        | None ->
            Format.printf "Graph.find_bridges: node not found\n" ;
            raise Not_found
        | Some succs -> succs )
    in
    dfs v1 ;
    min !score (size - !score)
  in
  match bridges with
  | [] -> None
  | e :: t ->
      let e_score = score e in
      let best_edge, _best_score =
        List.fold_left
          (fun ((_e1, e1_score) as acc) e2 ->
            let e2_score = score e2 in
            if e1_score > e2_score then acc else (e2, e2_score) )
          (e, e_score) t
      in
      Some best_edge

(* computes the bridges and then finds the best among them *)
let articulation (graph : ('a, 'b) t) =
  let bridges = find_bridges graph in
  central_bridge graph bridges

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
