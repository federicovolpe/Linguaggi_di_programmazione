module Matrix = struct                                                              
                                                                                    
  type matrix = int list list                                                         
                                                                                      
  (* funzione che controlla che una prima lista sia equivalente a una seconda lista *) 
  let rec list_eq l1 l2 =                                                             
    match l1, l2 with                                                                 
    | [], [] -> true                                                                  
    | h1 :: t1 , h2 :: t2 when h1 = h2 -> list_eq t1 t2                               
    | _ , _ -> false                                                                                 
                                                                                                                      
  (** funzione che controlla se una matrice m1 è equivalente a una seconda matrice m2 *)                                 
  let rec matrix_equivalence m1 m2 =                                                                                  
    match m1, m2 with                                                                                                 
    | [], [] -> true                                                                                                  
    | h1 :: t1, h2 :: t2 when list_eq h1 h2 -> matrix_equivalence t1 t2                                               
    | _, _ -> false                                                                                                   
                                                                                                                      
                                                                                                                      
  (** funzione che restituisce una matrice uguale a quella passata per argomento, non so se ho capito male la consegna *)                                    
  let matrix_copy (m: int list list) : int list list =                                                                
    List.map (fun riga -> List.map(fun elem -> elem) riga) m                                                          
                                                                                                                      
  (** funzione che moltiplica un vettore ossia int list per uno scalare alpha*)                                                                                                                  
  let scalar_vector_multiplication v alpha =                                                                          
      let rec costruttore v alpha acc =                                                                               
        match v with                                                                                                  
        | [] -> acc                                                                                                   
        | h:: t -> costruttore t alpha (acc @ [h * alpha])                                                            
    in                                                                                                                
    costruttore v alpha []                                                           
                              
  (** funzione che moltiplica tutti gli elementi di una matrice per uno scalare alpha *)                                                                                     
  let scalar_matrix_multiplication m1 alpha =                                          
      let rec costruttore m1 alpha acc =                                                 
        match m1 with                                                                  
        | [] -> acc                                                                    
        | h :: t -> costruttore t alpha (acc @ [scalar_vector_multiplication h alpha])                                             
    in                                                                                 
    costruttore m1 alpha []                                                                                                                                                                                                                                                                               
                                                                                                                                                                                                                                                                                                          
  (** moltiplicazione fra due matrici, preso dall'esercizio n 3 della parte precendente *)                                                                                                                                                                                                                 
    let mult2Righe r1 r2 =                                                                                                                                                                                                                                                                                  
      let rec helper r1 r2 res =                                                                                                                                                                                                                                                                          
        match r1 with                                                                                                                                                                                                                                                                                       
        | [] -> res                                                                                                                                                                                                                                                                                         
        | h1 :: t1 -> helper t1 (List.tl r2) ((h1 * List.hd r2) + res)                                                                                                                                                                                                                                      
    in                                                                                                                                                                                                                                                                                                    
    helper r1 r2 0                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
                                                                                                                                                                                                                                                                                                          
  (** moltiplicazione di una riga di dimensione n per n colonne *)                                                                                                                                                                                                                                        
  let rigaxColonne r1 m2 : int list =                                                                                                                                                                                                                                                                     
      let rec helper r1 m2 res : int list =                                                                                                                                                                                                                                                               
        match m2 with                                                                                                                                                                                                                                                                                     
        | h :: t -> helper r1 t (res @ [mult2Righe r1 h])                                                                                                                                                                                                                                                 
        | _ -> res                                                                                                                                                                                                                                                                                        
    in                                                                                                                                                                                                                                                                                                    
    helper r1 m2 []                                                                                                                                                                                                                                                                                       
  ;;                                                                                                                                                                                                                                                                                                      
                                                                                                                                                        
  (** trasposizione della matrice oggetto *)                                                                                                             
  let rec matrix_transposition = function                                                                                                                                 
    | [] -> []                                                                                                                                        
    | [] :: xss  -> matrix_transposition xss                                                                                                          
    | (x :: xs) :: xxs -> (x:: List.map List.hd xxs) :: matrix_transposition (xs::List.map List.tl xxs)                                                    
                                                                                   
  (** funzione che stabilise se la somma di una lista e' maggiore di un numero *)
  let higher (l: int list) (n : int) : bool =                                                        
    (List.fold_left (fun acc x -> acc + x) 0 l) > n                                                                                                           
                                                                                                        
  (** norma 1 della matrice oggetto ovvero la colonna con la somma maggiore *)                                                                           
  let matrix_norm m : int =                                                                                  
      let rec max (m: int list list) (prev: int) : int =                                                                                 
        match m with                                                                                    
        | [] -> prev                                                                                    
        | h :: t when higher h prev -> max t (List.fold_left (fun acc x -> acc + x) 0 h)                  
        | _ :: t -> max t prev                                                                          
    in                                                                                                  
    max (matrix_transposition m) 0                                                                                                                                                                                                                
                                                                                                                                                        
end ;;                                                                                                  
                                                                                                        
(* TESTING... *)                                                                                           
Matrix.list_eq [1;2;3;4;5;6] [1;2;3;4;5;6] ;;                                                                  
Matrix.list_eq [1;2;3;4;5;6] [1;2;3;4;5] ;;                                                                    
Matrix.list_eq [1;2;3;4;5;6] [] ;;                                                    
                                                                     
Matrix.matrix_copy [[1;2;3];[4;5;6];[7;8;9]] ;;                                          
                                                                                                        
Matrix.scalar_vector_multiplication [1;2;3;4;5] 0 ;;                                                           
Matrix.scalar_vector_multiplication [1;2;3;4;5] 5 ;;                                                           
                                                                               
Matrix.scalar_matrix_multiplication [[1;2;3];[4;5;6];[7;8;9]] 0 ;;                    
Matrix.scalar_matrix_multiplication [[1;2;3];[4;5;6];[7;8;9]] 2 ;;                    
                                                                               
Matrix.matrix_transposition [[1;2;3];[4;5;6];[7;8;9]] ;;                              
Matrix.matrix_transposition [[0]] ;;                                                  
                                                                               
Matrix.matrix_norm [[1;2;3];[4;5;6];[7;8;9]] ;;                                       
Matrix.matrix_norm [[0]] ;;                                                           
                                                                               