(** ESERCIZIO FUNZIONANTE                                                     
    realizzare un indice kwic seguendo le indicazioni del professore            
    deciso di implementare tramite una serie di funzioni                        
    seguendo i singoli punti del professore                                     
    *)                                                                          
                                                                                
(* funzione che rimuove gli spazi finali o iniziali o i doppi spazi             
   ne ritorna una lista delle parole significative del titolo*)                 
let clean_title title : string list =                                           
  (* rimozione degli spazi *)                                                   
  let tmp = List.filter(fun parola -> not (parola = "")) (String.split_on_char ' ' title)  
  in                                                                            
   (*tolgo le parole di poco conto *)                                           
  List.filter(fun parola -> not (List.mem parola ["The" ; "In" ; "Of"])) tmp    
;;                                                                              
                                                                                
(* dato un titolo crea le coppie *)                                             
let indicizza_titolo (title: string) n =                                        
  List.map(fun parola -> (n, parola, title)) (clean_title title)                   
;;                                                                              
                                                                                
  (* per ogni titolo creo una tupla (indice del titolo, parola da indicizzare)*)
let indicizza titles =                                                          
  let rec aux titles n =                                                                   
    match titles with                                                                      
    | [] -> []                                                                             
    | t :: rest -> (indicizza_titolo t n) @ aux rest (n+1)                                 
                                                                                           
  in                                                                                       
  aux titles 1                                                                                            
;;                                                                                                        
                                                                                                          
                                                                                           
(* ordinamento della lista di tuple in base all'ordine alfabetico della parola interessata 
   ovvero il secondo elemento della tupla *)                                        
let sort parole =                                                                                         
  let sort_logic (a1, a2, _) (b1, b2, _) = String.compare a2 b2                                           
  in                                                                                                      
  List.sort sort_logic parole                                                                             
;;                                                                                                        
                                                                                    
(* print dell'indice e del titolo delle tuple create                                
   idealmente da stampare allineati alla parola par cui vengono indicizzati: da fare*)
let print titoli =                                                                                        
                                                                                                          
  let indicizzati = sort (indicizza titoli)                                                                      
  in                                                                                                      
  List.map (fun (indice, parola, titolo) -> Printf.printf "%d               %s\n" indice titolo) indicizzati
;;                                                                                                        
                                                                                                          
                                                                                                          
                                                                                                          
                                                                                                          
(* TESTING... *)                                                                                          
let testi = ["Casablanca" ; "The Maltese Falcon" ; "The Big Sleep"] ;;                                    
print testi ;;                                                                                      