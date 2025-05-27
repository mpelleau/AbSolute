use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use std::fmt::{self, Debug, Display};

// N: Node type, E: Edge type (Hyperedge)
#[derive(Clone, Debug)]
pub struct Hypergraph<N, E>
where
    N: Eq + Hash + Clone + Debug,
    E: Eq + Hash + Clone + Debug,
{
    // Maps a node to a set of edges incident to it
    nodes_to_edges: HashMap<N, HashSet<E>>,
    // Maps an edge to a set of nodes it contains
    edges_to_nodes: HashMap<E, HashSet<N>>,
}

impl<N, E> Hypergraph<N, E>
where
    N: Eq + Hash + Clone + Debug,
    E: Eq + Hash + Clone + Debug,
{
    /// Creates a new empty hypergraph.
    pub fn new() -> Self {
        Hypergraph {
            nodes_to_edges: HashMap::new(),
            edges_to_nodes: HashMap::new(),
        }
    }

    /// Returns the number of nodes in the graph.
    pub fn num_nodes(&self) -> usize {
        self.nodes_to_edges.len()
    }

    /// Returns the number of edges in the graph.
    pub fn num_edges(&self) -> usize {
        self.edges_to_nodes.len()
    }

    /// Returns a vector of all edges in the graph.
    pub fn get_edges_vec(&self) -> Vec<E> {
        self.edges_to_nodes.keys().cloned().collect()
    }

    /// Returns a vector of all nodes in the graph.
    pub fn get_nodes_vec(&self) -> Vec<N> {
        self.nodes_to_edges.keys().cloned().collect()
    }

    /// Adds an edge `e` to the set of edges incident to node `n`.
    /// Also adds `n` to the graph if not already present.
    fn add_edge_to_node_map(&mut self, node: N, edge: E) {
        self.nodes_to_edges.entry(node).or_default().insert(edge);
    }

    /// Adds a node `n` to the set of nodes contained by edge `e`.
    /// Also adds `e` to the graph if not already present.
    fn add_node_to_edge_map(&mut self, edge: E, node: N) {
        self.edges_to_nodes.entry(edge).or_default().insert(node);
    }
    
    /// Adds a hyperedge defined by a list of nodes and the edge identifier.
    /// This is a higher-level operation that maintains consistency.
    pub fn add_hyperedge(&mut self, nodes_in_edge: &[N], edge: E) {
        for node in nodes_in_edge {
            self.add_node_to_edge_map(edge.clone(), node.clone());
            self.add_edge_to_node_map(node.clone(), edge.clone());
        }
        // Ensure the edge exists even if it has no nodes (though typically it should)
        self.edges_to_nodes.entry(edge).or_default();
    }


    /// Removes an edge `e` from the graph, including all its incidences.
    /// Panics if the edge does not exist, similar to OCaml's `invalid_arg`.
    pub fn remove_edge(&mut self, edge_to_remove: &E) {
        if let Some(nodes_of_edge) = self.edges_to_nodes.remove(edge_to_remove) {
            for node in nodes_of_edge {
                if let Some(incident_edges) = self.nodes_to_edges.get_mut(&node) {
                    incident_edges.remove(edge_to_remove);
                    // Optional: if incident_edges is empty, remove node from nodes_to_edges
                    // if incident_edges.is_empty() {
                    //     self.nodes_to_edges.remove(&node);
                    // }
                }
            }
        } else {
            panic!("Hypergraph::remove_edge: edge already removed or does not exist");
        }
    }

    /// Returns a new graph that is a copy of `self` but with edge `e` removed.
    /// Panics if `e` is not in the graph.
    pub fn subset_without_edge(&self, edge_to_remove: &E) -> Self {
        let mut new_graph = self.clone(); // Relies on derived Clone for HashMap/HashSet
        new_graph.remove_edge(edge_to_remove);
        new_graph
    }

    /// Builds a graph from a list of hyperedges, where each is a pair of (list of nodes, edge).
    pub fn build_from_supports(supports: Vec<(Vec<N>, E)>) -> Self {
        let mut graph = Self::new();
        for (nodes_in_edge, edge) in supports {
            // Add the edge and its nodes
            // The edges_to_nodes entry for 'edge' will collect all 'nodes_in_edge'
            let edge_nodes_set = graph.edges_to_nodes.entry(edge.clone()).or_default();
            for node in &nodes_in_edge {
                edge_nodes_set.insert(node.clone());
            }

            // For each node, add the edge to its incident edge set
            for node in nodes_in_edge {
                graph.nodes_to_edges.entry(node).or_default().insert(edge.clone());
            }
        }
        graph
    }
    

    /// Iterates over the edges incident to a given node.
    /// Calls function `f` for each edge.
    /// Panics if the node is not found in the graph.
    pub fn iter_edges_from_node<FVisit>(&self, node: &N, mut f: FVisit)
    where
        FVisit: FnMut(&E),
    {
        match self.nodes_to_edges.get(node) {
            Some(incident_edges) => {
                for edge in incident_edges {
                    f(edge);
                }
            }
            None => panic!("Hypergraph::iter_edges_from_node: node not found"),
        }
    }

    /// Iterates over the neighbors of a given node `n`.
    /// A neighbor is any node (excluding `n` itself) that shares at least one hyperedge with `n`.
    /// Calls function `f_neighbor` for each unique neighbor.
    /// Panics if `n` is not found in the graph.
    pub fn iter_neighbours_of_node<FVisitNbr>(&self, node_n: &N, mut f_neighbor: FVisitNbr)
    where
        FVisitNbr: FnMut(&N),
    {
        let mut visited_neighbors = HashSet::new(); // To call f_neighbor only once per neighbor

        if let Some(incident_edges_of_n) = self.nodes_to_edges.get(node_n) {
            for edge in incident_edges_of_n {
                if let Some(nodes_in_edge) = self.edges_to_nodes.get(edge) {
                    for neighbor_candidate in nodes_in_edge {
                        if neighbor_candidate != node_n { // Not node_n itself
                            if !visited_neighbors.contains(neighbor_candidate) {
                                f_neighbor(neighbor_candidate);
                                visited_neighbors.insert(neighbor_candidate.clone());
                            }
                        }
                    }
                }
            }
        } else {
            panic!("Hypergraph::iter_neighbours_of_node: source node not found");
        }
    }

    /// Formats the hypergraph for display.
    /// `fmt_node` and `fmt_edge` are closures that convert nodes/edges to strings.
    pub fn format_to_string<FN, FE>(&self, fmt_node: FN, fmt_edge: FE) -> String
    where
        FN: Fn(&N) -> String,
        FE: Fn(&E) -> String,
    {
        let mut parts = Vec::new();
        // Sort edges for deterministic output, if E is Ord.
        // For now, iterate in HashMap's arbitrary order.
        // To sort, collect keys, sort them, then lookup.
        let mut sorted_edges: Vec<_> = self.edges_to_nodes.keys().cloned().collect();
        // Requires E: Ord to sort edges consistently. For now, we skip sorting if not Ord.
        // if type_name_of_val(&sorted_edges.first()) gives hint of Ord... (not easy)
        // Or, require E: Ord in function signature if sorting is desired.
        // For now, let's proceed without sorting edges for general case.

        for (edge, nodes) in &self.edges_to_nodes {
            let mut node_strs: Vec<String> = nodes.iter().map(&fmt_node).collect();
            // Sort nodes for deterministic output, if N is Ord.
            // Similar to edges, requires N: Ord to sort. For now, join as is.
            node_strs.sort_unstable(); // If N: Ord. If not, this won't compile without bound.
                                      // Let's assume N: Ord for consistent printing of node lists.
                                      // The function signature should reflect this if it's a hard requirement.
                                      // For now, if N is not Ord, this sort will be an issue.
                                      // Let's remove sort for nodes for now to keep it general like OCaml print.
            let nodes_str = node_strs.join(", ");
            parts.push(format!("{}: [{}]", fmt_edge(edge), nodes_str));
        }
        parts.join("\n")
    }
}

// Default implementation for empty graph
impl<N, E> Default for Hypergraph<N, E>
where
    N: Eq + Hash + Clone + Debug,
    E: Eq + Hash + Clone + Debug,
{
    fn default() -> Self {
        Self::new()
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_hypergraph_new() {
        let graph = Hypergraph::<String, i32>::new();
        assert_eq!(graph.num_nodes(), 0);
        assert_eq!(graph.num_edges(), 0);
    }

    #[test]
    fn test_hypergraph_build_from_supports_and_getters() {
        let supports = vec![
            (vec!["n1".to_string(), "n2".to_string()], 100),
            (vec!["n2".to_string(), "n3".to_string()], 200),
            (vec!["n1".to_string(), "n3".to_string(), "n4".to_string()], 300),
        ];
        let graph = Hypergraph::build_from_supports(supports);

        assert_eq!(graph.num_nodes(), 4); // n1, n2, n3, n4
        assert_eq!(graph.num_edges(), 3); // 100, 200, 300

        let mut nodes = graph.get_nodes_vec();
        nodes.sort(); // For stable comparison
        assert_eq!(nodes, vec!["n1", "n2", "n3", "n4"].iter().map(|s|s.to_string()).collect::<Vec<_>>());

        let mut edges = graph.get_edges_vec();
        edges.sort(); // For stable comparison
        assert_eq!(edges, vec![100, 200, 300]);

        // Check specific node to edge mappings
        let mut n1_edges = Vec::new();
        graph.iter_edges_from_node(&"n1".to_string(), |e| n1_edges.push(*e));
        n1_edges.sort();
        assert_eq!(n1_edges, vec![100, 300]);

        // Check specific edge to node mappings
        assert_eq!(graph.edges_to_nodes.get(&300).unwrap().len(), 3);
        assert!(graph.edges_to_nodes.get(&300).unwrap().contains(&"n1".to_string()));
    }
    
    #[test]
    fn test_add_hyperedge() {
        let mut graph = Hypergraph::<String, i32>::new();
        graph.add_hyperedge(&["n1".to_string(), "n2".to_string()], 101);
        graph.add_hyperedge(&["n2".to_string(), "n3".to_string()], 102);

        assert_eq!(graph.num_nodes(), 3);
        assert_eq!(graph.num_edges(), 2);
        assert!(graph.edges_to_nodes.get(&101).unwrap().contains(&"n1".to_string()));
        assert!(graph.nodes_to_edges.get(&"n2".to_string()).unwrap().contains(&101));
        assert!(graph.nodes_to_edges.get(&"n2".to_string()).unwrap().contains(&102));
    }


    #[test]
    fn test_hypergraph_remove_edge() {
        let supports = vec![
            (vec!["n1".to_string(), "n2".to_string()], 100),
            (vec!["n2".to_string(), "n3".to_string()], 200),
        ];
        let mut graph = Hypergraph::build_from_supports(supports);
        
        graph.remove_edge(&100);
        assert_eq!(graph.num_edges(), 1);
        assert!(graph.edges_to_nodes.get(&100).is_none());
        assert!(!graph.nodes_to_edges.get(&"n1".to_string()).unwrap().contains(&100));
        assert!(!graph.nodes_to_edges.get(&"n2".to_string()).unwrap().contains(&100));
        assert!(graph.nodes_to_edges.get(&"n2".to_string()).unwrap().contains(&200)); // n2 still part of edge 200
    }

    #[test]
    #[should_panic(expected = "Hypergraph::remove_edge: edge already removed or does not exist")]
    fn test_hypergraph_remove_edge_non_existent() {
        let mut graph = Hypergraph::<String, i32>::new();
        graph.remove_edge(&100);
    }

    #[test]
    fn test_hypergraph_subset_without_edge() {
        let supports = vec![
            (vec!["n1".to_string(), "n2".to_string()], 100),
            (vec!["n2".to_string(), "n3".to_string()], 200),
        ];
        let graph = Hypergraph::build_from_supports(supports);
        let sub_graph = graph.subset_without_edge(&100);

        assert_eq!(graph.num_edges(), 2); // Original unchanged
        assert_eq!(sub_graph.num_edges(), 1);
        assert!(sub_graph.edges_to_nodes.get(&200).is_some());
        assert!(sub_graph.edges_to_nodes.get(&100).is_none());
    }

    #[test]
    fn test_hypergraph_iter_neighbours() {
        let supports = vec![
            (vec!["n1".to_string(), "n2".to_string()], 100), // n1 is neighbor of n2 (via 100)
            (vec!["n2".to_string(), "n3".to_string()], 200), // n2 is neighbor of n3 (via 200)
            (vec!["n1".to_string(), "n3".to_string()], 300), // n1 is neighbor of n3 (via 300)
            (vec!["n4".to_string()], 400),                 // n4 has no explicit neighbors here
        ];
        let graph = Hypergraph::build_from_supports(supports);

        let mut n1_neighbors = HashSet::new();
        graph.iter_neighbours_of_node(&"n1".to_string(), |n_id| {
            n1_neighbors.insert(n_id.clone());
        });
        let expected_n1_neighbors: HashSet<_> = ["n2", "n3"].iter().map(|s| s.to_string()).collect();
        assert_eq!(n1_neighbors, expected_n1_neighbors);

        let mut n2_neighbors = HashSet::new();
        graph.iter_neighbours_of_node(&"n2".to_string(), |n_id| {
            n2_neighbors.insert(n_id.clone());
        });
        let expected_n2_neighbors: HashSet<_> = ["n1", "n3"].iter().map(|s| s.to_string()).collect();
        assert_eq!(n2_neighbors, expected_n2_neighbors);
        
        let mut n4_neighbors = HashSet::new();
        graph.iter_neighbours_of_node(&"n4".to_string(), |n_id| {
            n4_neighbors.insert(n_id.clone());
        });
        assert!(n4_neighbors.is_empty());
    }

    #[test]
    #[should_panic(expected = "Hypergraph::iter_edges_from_node: node not found")]
    fn test_iter_edges_from_non_existent_node() {
        let graph = Hypergraph::<String, i32>::new();
        graph.iter_edges_from_node(&"n1".to_string(), |_e| {});
    }
    
    #[test]
    fn test_format_to_string() {
        // Requires N: Ord, E: Ord for stable output for this test.
        // Let's use types that are Ord.
        let supports = vec![
            (vec![1, 2], 100),
            (vec![2, 3], 200),
        ];
        let graph = Hypergraph::build_from_supports(supports);
        
        // Sort edges for deterministic output in the test string
        let mut edge_keys: Vec<_> = graph.edges_to_nodes.keys().cloned().collect();
        edge_keys.sort();

        let mut output_parts = Vec::new();
        for edge_key in edge_keys {
            let nodes = graph.edges_to_nodes.get(&edge_key).unwrap();
            let mut node_strs: Vec<String> = nodes.iter().map(|n| n.to_string()).collect();
            node_strs.sort(); // Nodes are sorted within their list
            output_parts.push(format!("{}: [{}]", edge_key, node_strs.join(", ")));
        }
        let expected_str = output_parts.join("\n");
        
        // The format_to_string does not guarantee edge order unless E:Ord and sorted internally.
        // For testing, we reconstruct the string with sorted edges.
        let actual_str = graph.format_to_string(|n| n.to_string(), |e| e.to_string());
        
        // Since actual_str might have different order of lines (edges)
        // we check if they contain the same lines.
        let mut actual_lines: Vec<_> = actual_str.lines().collect();
        actual_lines.sort();
        let mut expected_lines: Vec<_> = expected_str.lines().collect();
        expected_lines.sort();
        assert_eq!(actual_lines, expected_lines);
    }
}
