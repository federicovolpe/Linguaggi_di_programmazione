(*Esercizio non ancora inizializzato *)                                                                
                                                                                                       
(**funzione che data un numero rappresentante la iterazione e la variabile x ne ritorna il risultato *)
let single_iteration_sin (iter: int) (x: int) : float=                                                 
  match (iter mod 2) with                                                                              
  | 0 -> (*iterazione con il segno -*)  0.                                                             
  | _ -> (*iterazione con il segno +*)  0.                                                             
;;                                                                                                     
                                                                                                       
let approximation_sin (iter: int) (x: int) : float =                                                   
    let rec helper i x res =                                                                           
      match i with                                                                                     
      | 0 -> (*ritorno il risultato ottenuto fino ad ora*) 0.                                          
      | n -> (*calcolo e sommo la iterazione successiva*)  0.  
  in                                                         
  helper iter x 0                                               
;;                                                           
                                                             
let choose_fun (f: string) (iterazioni: int) (x: int) : float =                         
  match f with                                               
  | "sin"   -> approximation_sin iterazioni x                                              
  | "cosin" -> approximation_sin iterazioni x                
  | "tan"   -> approximation_sin iterazioni x                
  | "log"   -> approximation_sin iterazioni x                
  | _ -> print_string "function not yet implemented"; 0.     
;;                                                           