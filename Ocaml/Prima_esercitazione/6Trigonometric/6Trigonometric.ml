let rec factorial = function                                                                                                       
  | 0 -> 1                                                                                                                         
  | 1 -> 1                                                                                                                         
  | n -> n + factorial (n-1)                                                                                                       
;;                                                                                                                                 
                                                                                                                                   
(**funzioni che data un numero rappresentante la iterazione e la variabile x ne ritornano il risultato *)                          
let taylor_sin (iter: int) (x: int) : float =                                                                                      
  let meta_risultato = float_of_int x ** float_of_int (2 * iter + 1) /. float_of_int (factorial (2 * iter + 1)) in                                                  
  Printf.printf "iterazione %d = %f\n" iter meta_risultato;                                                                          
                                                                                                                                   
  match (iter mod 2) with                                                                                                          
  | 0 -> -. meta_risultato (*iterazione con il segno -*)                                                                           
  | _ -> meta_risultato (*iterazione con il segno +*)                                                                                          
;;                                                                                                                                 
                                                                                                                                   
let taylor_cos (iter: int) (x: int) : float =                                                                                      
  match (iter mod 2) with                                                                                                          
  | 0 -> (*iterazione con il segno -*)  0.                                                                                         
  | _ -> (*iterazione con il segno +*)  0.                                                                                         
;;                                                                                                                                 
                                                                                                                                   
let taylor_tan (iter: int) (x: int) : float =                                                                                      
  match (iter mod 2) with                                                                                                          
  | 0 -> (*iterazione con il segno -*)  0.                                                                                         
  | _ -> (*iterazione con il segno +*)  0.                                                                                         
;;                                                                                                                                 
                                                                                                                                   
let taylor_log (iter: int) (x: int) : float =                                                                                      
  match (iter mod 2) with                                                                                                          
  | 0 -> (*iterazione con il segno -*)  0.                                                                                         
  | _ -> (*iterazione con il segno +*)  0.                                                                                         
;;                                                                                                       
                                                                                                         
(** funzione che calcola la somma delle iterazioni della approssimazione della funzione passata *)
let approximation (f:int -> int -> float) (iter: int) (x: int) : float =                                                     
    let rec helper i x res:float =                                                                           
      match i with                                                                                       
      | 0 -> res                                                                                         
      | _ -> helper (i-1) x (res +. (f i x)) (*calcolo e sommo la iterazione successiva*)                                  
  in                                                                                                     
  helper iter x 0.                                                                                       
;;                                                                                                       
                                                                                                        
(** funzione che in base alla funzione passata per argomento ritorna la sua approssimazione di taylor *)
let choose_fun (f: string) (iterazioni: int) (x: int) : float =                                          
  match f with                                                                                          
  | "sin"   -> approximation taylor_sin iterazioni x                                                    
  | "cosin" -> approximation taylor_cos iterazioni x                                                    
  | "tan"   -> approximation taylor_tan iterazioni x                                                    
  | "log"   -> approximation taylor_log iterazioni x                                                    
  | _ -> print_string "function not yet implemented"; 0.                                                
;;                                                                                                      