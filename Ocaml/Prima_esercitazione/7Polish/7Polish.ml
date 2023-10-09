(*L'esercizio viene risolto con l'utilizzo dello Shunting Yard algorithm per la traduzione
   da notazione classica a polish *)         
                                   
type expr =                                                                             
  |Num of int                                                                           
  |Add of expr * expr                                                                   
  |Sub of expr * expr                                                                   
  |Mul of expr * expr                                                                   
  |Div of expr * expr                                                                   
  |Pow of expr * expr                                                                   
;;                                                                                      
                                                                                        
(**funzione che trasforma la stringa data in input in una espressione in forma polish *)
let exp s =                                                                             
    let rec expr_of_string (s: string list): expr =                                     
      match s with                                                                     
      | [] -> failwith "Empty expression string"                                        
      | "(" :: e :: ")" :: t -> expr_of_string (e :: t)                                       
      | [x] -> Num (int_of_string x)                                                    
      | h :: t ->                                                                       
              match h with                                                              
                | "+" -> Add (expr_of_string t, expr_of_string [])                      
                | "-" -> Sub (expr_of_string t, expr_of_string [])                      
                | "*" -> Mul (expr_of_string t, expr_of_string [])                      
                | "/" -> Div (expr_of_string t, expr_of_string [])      
                | "^" -> Pow (expr_of_string t, expr_of_string [])      
                | _ -> failwith ("Unknown operator " ^ h)               
  in                                                                    
  expr_of_string (String.split_on_char ' ' s)                           
;;                                                                      
                                                                        
let rec eval (exp: expr): int =                                         
  match exp with                                                        
  | Num n -> n                                                          
  | Add (e1, e2) -> eval e1 + eval e2                                   
  | Sub (e1, e2) -> eval e1 - eval e2                                   
  | Mul (e1, e2) -> eval e1 * eval e2                                   
  | Div (e1, e2) -> eval e1 / eval e2                                   
  | Pow (e1, e2) -> int_of_float (float_of_int (eval e1) ** float_of_int (eval e2))                  
;;                                                                      
                                                                        
(*testing*)                             
let espressione = exp "( 3 + 4 ) * 5" ;;