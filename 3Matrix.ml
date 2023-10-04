type matrix = int list list;;                                                  
                                                                               
let rec zeroes n m =                                                           
  (**costruisce una matrice con tutti zero di dimensione n*)                   
  List.init n (fun x ->                                                           
  List.init m (fun x -> 0));;                                                  
                                                                               
                                                                               
                                                                               
let identity size =                                                            
  (** costruisce una matrice identita' di dimensione n*)                       
  List.init size (fun x -> List.init size (fun y -> if y == x then 1 else 0));;                
                                                                                          
let init n =                                                                              
  List.init n (fun x -> List.init n (fun y -> x*y));;                                                                 
                                                                                          
let rec transpose =                                                                     
  function                                                                               
    | [] -> []                                                                            
    | [] :: xss  -> transpose xss                                                          
    | (x::xs):: xxs -> (x:: List.map List.hd xxs)::transpose (xs::List.map List.tl xxs);;                                                         
                                                                                          
let + m1 m2 =                                                                             
                                                                                          
let * m1 m2 =                                                                             
                                                                                          
                                                                                          
[0; 0; 0; 0;  0 ];                                                                        
[0; 1; 2; 3;  4 ];                                                                        
[0; 2; 4; 6;  8 ];                                                                        
[0; 3; 6; 9;  12];                                                                        
[0; 4; 8; 12; 16]                                                                         
# transpose m;;                                                                           
- : int list list =                                                                       
[0; 0; 0; 0; 0];                                                                          
[0; 1; 2; 3; 4];                                                                          
[0; 2; 4; 6; 8];                                                                          
[0; 3; 6; 9; 12];                                                                         
[0; 4; 8; 12; 16]]                                                                        