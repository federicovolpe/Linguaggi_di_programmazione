(* gli individui sono rappresentati dai nodi
possono essere connessi attraverso legami di *)
module RelationsGraph = struct
  type node = string
  type edge = node * node
  type graph = edge list
  let create : graph = []

  (* aggiunge un nodo alla struttura dati corrente *)
  let rec add_edge (node1, node2) (g: graph) = 
    (node1, node2):: (node2, node1) :: g  

  (* per un dato nodo ritorna una rappresentazione grafica di nodo -> nodi collegati *)
  let rec get_neighbors (node: node) g =
    let nodi_interessati = List.filter (fun (n, n2) -> n = node) g
    in 
    [node] @ List.map(fun (n1, n2) -> n2) nodi_interessati

end

(* testing *)
let dummy = RelationsGraph.create ;;

let dummy = RelationsGraph.add_edge ("A", "B") dummy ;;
let dummy = RelationsGraph.add_edge ("A", "C") dummy ;;
let dummy = RelationsGraph.add_edge ("B", "C") dummy ;;
let dummy = RelationsGraph.add_edge ("B", "D") dummy ;;
let dummy = RelationsGraph.add_edge ("C", "D") dummy ;;

print_string "nodi adiacenti ad A: \n" ;;
RelationsGraph.get_neighbors "D" dummy ;;

dummy;;