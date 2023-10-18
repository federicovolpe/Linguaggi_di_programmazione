(** interfaccia delle matrici con le funzioni richieste *)

module type Matrix = sig                                  
                                                          
  type m = int list list                                  
                                                          
  (* ritorna true se le due matrici sono uguali *)                             
  val matrix_equivalence : m -> m -> bool                 
                                                          
  (* date due matrici rende la seconda uguale alla prima *)                  
  val matrix_copy : m -> m -> m                                              
                                                                             
  (* moltiplicazione fra una matrice e un vettore *)                         
  val scalar_matrix_multiplication : m -> int list -> m                      
                                                                             
  (* moltiplicazione fra due matrici *)                                      
  val matrix_matrix_multiplication : m -> m -> m                             
                                                                             
  (* trasposizione della matrice oggetto *)                                  
  val matrix_transposition : m -> m                                          
                                                                             
  (* norma 1 della matrice oggetto ovvero la colonna con la somma maggiore *)
  val matrix_norm : m -> int                                                 
                                                                             
end                                                                          
;;                                       
  
  