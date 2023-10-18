(** ESERCIZIO TERMINATO
    modulo per la gestione delle operazioni dei grafi 
    - creazione
    - aggiunta di archi di relazioni
    - nodi adiacenti            
    *)                          
module RelationsGraph = struct  
  (* definizione dei tipi utili alla gestione del grafo *)
  type node = string            
  type edge = node * node       
  type graph = edge list        
                                
  (* creazione del grafo vuoto, come puntatore poiche' verra' cambiato il riferimento con le aggiunte degli archi*)
  let create : graph ref = ref [] 
                                
  (* aggiunge i due archi ai nodi corrispondenti e sposta il puntatore del grafo *)
  let add_edge (node1, node2) g =
    g := (node1, node2) :: (node2, node1) :: !g 
                                
  (* per un dato nodo ritorna una rappresentazione grafica di nodo -> nodi collegati *)
  let rec get_neighbors (node: node) g =
    let nodi_interessati = List.filter (fun (n, n2) -> n = node) !g
    in                          
    [node] @ List.map(fun (n1, n2) -> n2) nodi_interessati
                
end             
                
(* TESTING... *)  
                  
(*crazione del grafo *) 
let dummy = RelationsGraph.create ;;
(*aggiunta degli archi del grafo*)       
RelationsGraph.add_edge ("A", "B") dummy ;;       
RelationsGraph.add_edge ("A", "C") dummy ;;       
RelationsGraph.add_edge ("B", "C") dummy ;;       
RelationsGraph.add_edge ("B", "D") dummy ;;       
RelationsGraph.add_edge ("C", "D") dummy ;;       
RelationsGraph.add_edge ("a", "D") dummy ;;       
                                                  
print_string "nodi adiacenti ad A: \n" ;;         
RelationsGraph.get_neighbors "D" dummy ;;         
                                                  
(* output:                                        
   RelationsGraph.node list = ["D"; "a"; "C"; "B"]
   *)                                             