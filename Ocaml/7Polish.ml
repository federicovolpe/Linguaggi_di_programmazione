(*Esercizio non ancora terminato *)                                           
module Polish = struct                                    
    (*                                                                                  
    type operation =                                                                  
      | "+" -> fun x y -> x + y                                                                           
      | "-" -> fun x y -> x - y                                                       
      | "*" -> fun x y -> x * y                                                       
      | "/" -> fun x y -> x / y                                                       
                                                                                      
    type number = int ;;                                                               
                                                                                      
    type expr = (number | operation) Stack.t                                                   
                                                                                      
    (**presa una stringa di una espressione la trasforma in una espressione*)         
    let expr_of (s: string): expr = function                                          
    | number -> (*adds the number to the resulting expr*)                             
    | operation -> (*adds the operator to the expr*)                                                          
                                                                                      
    (**function that evaluates the function created*)                                 
    let eval (e: expr): int =                                                         
      match e with                                                                    
      | Stack vuoto ->                                                                
      | operation :: resto ->                                                         
     *)                                                                                 
end;;                                                                                 