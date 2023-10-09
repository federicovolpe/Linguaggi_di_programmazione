(** returns if a number is prime *)
let is_prime x =                                    
  if x = 1 then true                                
  else                                              
    let rec aux x div =                                                                
      match x mod div with                                                                                                                  
      | 0 when div = x -> true                                                                                                              
      | 0 -> false                                                                                                                          
      | _ -> aux x (div + 1)                                                                                                                
  in                                                                                                                                        
  aux x 2                                                                                                                                   
;;                                                                                                                                          
                                                                                                                                            
(** ritorna la coppia di numeri primi che sommati danno n (la partizione di goldbach) *)                                                    
let rec g_partition n first : int list = (* serve il contatore first per sapere il numero primo candidato per essere il primo della coppia*)                                                                                    
  if first > n then [0;0] (* numero troppo alto per essere candidato *)                                                                                                                 
  else                                                                                                                                      
    if is_prime first && is_prime (n - first) (* se il numero e' primo e sommato ad un altro risultano congruenti*)
      then [first ; (n - first)]                                                                                
      else g_partition n (first + 1) (* provo con il numero successivo *)                                                                                                          
;;                                                                                                                                          
                                                                                                                                            
                                                                                                                                            
(** that returns a Goldbach partition for n *)                                                                                              
let goldbach n =                                                                                                                            
  g_partition n 1                                                                                                                           
;;                                                                                                                           
                                                                                                                      
(** returns a list of Goldbach partitions for the even numbers in the range (n,m) *)                                                                                                      
let goldbach_list n m =                                                                
    let rec accumulator n m acc = (* accumulatore per le coppie di numeri create *)                                                    
      if n >= m then acc                                                               
      else                                                                             
        accumulator (n+1) m (acc @ [goldbach n])                                  
  in                                                                              
  accumulator n m []                                                              
;;                                                                                