module Matrix = struct                                                            
                                                                                  
type matrix = int list list                                                       
                                                                                  
(*funzione che controlla che una prima lista sia equivalente a una seconda lista*)
let rec list_eq (lista1 : int list) (lista2 : int list) : bool =                  
  match lista1 with                                                               
  |[] -> if List.length lista2 = 0 then true else false                           
  | h :: t -> match lista2 with                                                   
              | [] -> false                                                       
              | x :: y -> if x = h then list_eq t y else false                    
                                                                                  
(*funzione che controlla se una matrice m1 è equivalente a una seconda matrice m2*)
let rec matrix_equivalence (m1: matrix) (m2: matrix) =                            
  match m1 with                                                                   
  | [] -> if List.length m2 = 0 then true else false                              
  | h :: t -> match m2 with                                                       
              | [] -> false                                                       
              | x :: y -> if list_eq h x then matrix_equivalence t y else false   
                                                                                  
(*funzione che data una lista piena e una vuota riempie quella vuota con gli elementi della prima*)
let riempiRiga (lista1 : int list) (lista2 : int list) =                          
  let rec aux x =
    match x with
    | [] -> []
    | h :: t -> h :: aux t 
  in aux lista1

(*funzione che rende la matrice m2 una copia della matrice m1*)
let matrix_copy (m1 : matrix)(m2: matrix) =
  let b = m1    
  (*mi devo creare una funzione che crei una matrice nuova ogni volta che viene inserito un valore
  let aux a b = 
    match a with 
    | [] -> b   
    | h :: t -> *)
                
                
let matrix_addition =
                
                
                
end ;;          