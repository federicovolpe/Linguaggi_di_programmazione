type matrix = int list list;;                                                                 
                                                                                               
  (**costruisce una matrice con tutti zero di dimensione n*)                                   
let rec zeroes n m =                                                                           
  List.init n (fun x ->                                                                        
  List.init m (fun x -> 0))                                                                    
;;                                                                                             
                                                                                               
<<<<<<< HEAD
  (** costruisce una matrice identita' di dimensione n*)                                                               
let identity size =                                                                                                                 
  List.init size (fun x -> List.init size (fun y -> if y == x then 1 else 0))                                          
;;                                                                                                                     
                                                                                                                       
  (** costruisce una matrice "filled with the first n×n integers" *)                                                   
let init n =                                                                                                           
  List.init n (fun x -> List.init n (fun y -> x*y));;                                                                  
                                                                                                                       
=======
  (** costruisce una matrice identita' di dimensione n*)                                       
let identity size =                                                                                                                 
  List.init size (fun x -> List.init size (fun y -> if y == x then 1 else 0))                  
;;                                                                                             
                                                                                               
  (** costruisce una matrice "filled with the first n×n integers" *)                                              
let init n =                                                                                   
  List.init n (fun x -> List.init n (fun y -> x*y));;                                                                 
                                                                                       
>>>>>>> develop
  (** costruisce la matrice trasposta di quella passata per argomento *)                                                                   
let rec transpose =                                                                                                                        
  function                                                                                                                                 
    | [] -> []                                                                                                                             
    | [] :: xss  -> transpose xss                                                                                                          
    | (x::xs):: xxs -> (x:: List.map List.hd xxs)::transpose (xs::List.map List.tl xxs)                                                    
;;                                                                                                                                         
                                                                                                                                           
  (** costruisce una matrice identita' di dimensione n*)                                                                                   
let identity size =                                                                                                                                                                                                               
  List.init size (fun x -> List.init size (fun y -> if y == x then 1 else 0))                                                             
;;                                                                                                                                                                                                                                                                                                      
                                                                                                                                                                                                                                                                                                        
(*------------------------------------------------------ somma di due matrici -----------------------------------------------*)                                                                                                                                                                         
let (+:) (m1: int list list) (m2: int list list) : int list list =                                                                                                                                                                                                                                      
    let sommaRighe (r1: int list) (r2: int list) : int list =                                                                                                                                                                                                                                           
      List.map2(fun elem1 elem2 -> elem1 + elem2) r2 r1   (*map2 per il matching di 2 righe contemporaneamente*)                                                                                                                                                                                        
  in                                                                                                                                                                                                                                                                                                    
  List.map2(fun riga1 riga2 -> sommaRighe riga1 riga2) m2 m1                                                                                                                                                                                                                                            
;;                                                                                                                                                                                                                                                                                                      
                                                                                                                                                                                                                                                                                                        
(*------------------------------------------------------ moltiplicazione di due matrici -----------------------------------------------*)  
(** moltiplicazione di due list di numeri dato che non esiste una List.fold_left2 *)                                                                                                                                                             
let mult2Righe r1 r2 =                                                                                                                                                                                                                                                                                  
    let rec helper r1 r2 res =                                                                                                                                                                                                                                                                      
      match r1 with                                                                                                                                                                                                                                                                                       
      | [] -> res                                                                                                                                                                                                                                                                                         
      | h1 :: t1 -> helper t1 (List.tl r2) ((h1 * List.hd r2) + res)                                                                                                                                                                                                                                      
  in                                                                                                                                                                                                                                                                                                    
  helper r1 r2 0                                                                                                                                                                                                                                                                                        
;;                                                                                                                                                                                                                                                                                                      
                                                                   
  (** moltiplicazione di una riga di dimensione n per n colonne *) 
let rigaxColonne r1 m2 : int list =                                                                                                                                                                                                                                                                     
    let rec helper r1 m2 res : int list =                                                                                                                              
      match m2 with                                                                                                                                                
      | h :: t -> helper r1 t (res @ [mult2Righe r1 h])                                                                                                              
      | _ -> res                                                                                                                                                   
  in                                                                                                                                                             
  helper r1 m2 []                                                                                                                                                
;;                                                                                                                                                                                                                                                                                                                       
                                                                                                                                                                 
let x m1 m2 =                                                                                                                                                                                                         
    let rec helper m1 m2 res =                                                                                                                                                                                        
      match m1 with                                                                                                                                                                                                   
      | h1 :: t1 -> helper t1 m2 (res @ [rigaxColonne h1 m2])                                                                                                                                                
      | _ -> res                                                                                                                                                                                                      
  in                                                                                                                                                                                                                  
  helper m1 (transpose m2) []                                                                                                                                                                                         
;;                                                                                                                                                               
                                                                                                                                                                 
(*testing...*)                                                                                       
rigaxColonne [1;2;3] [[1;2;3];[1;2;3];[1;2;3]] ;;                                                                                                                                                                                                                      
x [[1;2;3];[4;5;6];[7;8;9]] [[1;2;3];[4;5;6];[7;8;9]] ;;                                          