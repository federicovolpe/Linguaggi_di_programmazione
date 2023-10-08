let rec factorial = function                                                                                                       
  | 0 -> 1                                                                                                                         
  | 1 -> 1                                                                                                                         
  | n -> n + factorial (n-1)                                                                                                       
;;                                                                                                                                 
                                                                                                                                   
(**funzioni che data un numero rappresentante la iterazione e la variabile x ne ritornano il risultato *)                          
let taylor_sin x n =                                                                    
  let rec iterazione x n acc sign =                                                     
    match n with                                                                        
    | 0 -> acc                                                                          
    | _ -> let term =                                                                                                              
                      (sign *.                                (*segno della iterazione*)                                           
                      (x ** (float_of_int (2 * n - 1))))      (*calcolo del numeratore*)                                           
                      /. float_of_int (factorial (2 * n - 1)) (*calcolo del denominatore*)                                         
                    in                                                                                                             
           iterazione x (n - 1) (acc +. term) (-. sign)                                                                            
  in                                                                                                                               
  iterazione x n 0.0 1.0                                                                                                           
;;                                                                                                                                 
                                                                                               
(* coseno, differisce per l'esponente del numeratore e dal denominatore rispetto al precendente*)
let taylor_cos x n =                                                                                                               
  let rec iterazione x n acc sign =                                                                                                
    match n with                                                                                                                   
    | 0 -> acc                                                                                                                     
    | _ -> let term =                                                                                                              
                      (sign *.                                (*segno della iterazione*)                                           
                      (x ** (float_of_int (2 * n))))          (*calcolo del numeratore*)                                           
                      /. float_of_int (factorial (2 * n ))    (*calcolo del denominatore*)                                         
                    in                                                                                                             
           iterazione x (n - 1) (acc +. term) (-. sign)                                                                            
  in                                                                                                                               
  iterazione x n 0.0 1.0                                                                                                           
;;                                                                                                                                  
                                                                                                                                   
let taylor_tan x n =                                                                    
  let rec iterazione x n acc =                                                     
    match n with                                                                        
    | 0 -> acc                                                                          
    | _ -> (*da implementare ...*) 0.                               
  in                                                                                    
  iterazione x n 0.0                                                                
;;                                                                                                                                  
                                                                                                                                   
let taylor_log x n =                                                                    
  let rec iterazione x n acc sign =                                                     
    match n with                                                                        
    | 0 -> acc                                                                                          
    | _ -> (*da implementare....*) 0.                                                
  in                                                                                                     
  iterazione x n 0.0 1.0                                                                                 
;;                                                                                                                                                                                                             
                                                                                                         
(** funzione che in base alla funzione passata per argomento ritorna la sua approssimazione di taylor *) 
let choose_fun (f: string) (iterazioni: int) (x: int) : float =                                          
  match f with                                                                                           
  | "sin"   -> taylor_sin (float_of_int iterazioni) x                                                     
  | "cosin" -> taylor_cos (float_of_int iterazioni) x                                                     
  | "tan"   -> taylor_tan (float_of_int iterazioni) x                                                     
  | "log"   -> taylor_log (float_of_int iterazioni) x                                                     
  | _ -> print_string "function not yet implemented"; 0.                                                 
;;                                                                                                       