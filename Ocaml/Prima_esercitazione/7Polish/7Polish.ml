(*Esercizio non ancora terminato !                      
                                                        
L'esercizio viene risolto con l'utilizzo dello Shunting Yard algorithm per la traduzione
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

(** toglie tutto quello che c'è nello stack operators fino alla prima parentesi aperta*)
let rec pop_until_parenthesis operators output =              
  let operator = Stack.pop operators in                   
  if operator <> "("                          
    then                                      
      Stack.push operator output;             
      pop_until_parenthesis operators output            
;;                                   
                                     
let precedence = function
  | "^" -> 4
  | "*"	-> 3
  | "/"	-> 3
  | "+" -> 2
  | "-" -> 2
  | _   -> 0
;;      
        
(** funzione che toglie gli operatori dallo stack operators che hanno una precedenza superiore a quella dell'operatore x*)
let rec ops (operators : string Stack.t) (output: string Stack.t) (x: string): unit =  
  if not (Stack.is_empty operators) 
    then     
        let precedenza_x = precedence (x) in
        let top_operator = precedence (Stack.top operators) in
                                           
        if Stack.is_empty operators && top_operator >= precedenza_x (*se la precedenza del top dello stack è maggiore allora lo tolgo*)                                               
          then                                             
            Stack.push (Stack.pop operators) output;       
            ops operators output x 
          else
            Stack.push x operators (* infine aggiungo l'elemento allo stack *)
    else ();
;;      
let a = Stack.create ();;
let b = Stack.create ();;
Stack.push "+" a;;
Stack.push "*" a;;
Stack.push "+" a;;
Stack.push "+" a;;  
ops a b "*";;
stack_to_list b;;                                          
                                                     
let rec svuota_op operators output =                 
  if Stack.is_empty operators                        
    then                                             
      Stack.push (Stack.pop operators) output;       
      svuota_op operators output;  
                
;;   

let stack_to_list stack = 
    let rec aux s acc =
      if Stack.is_empty s 
        then
          acc
        else
          let popped = Stack.pop s in
          aux s (popped :: acc)
  in
  aux stack []
;;                            

let rec shunting_yard (infix: string): string =                          
  let operators = Stack.create () in                                     
  let output = Stack.create () in

  let _ = List.map( fun x ->                                                      
    Printf.printf "trovato : %s\n" x;              
    match x with                                                    
                    | "(" -> 
                      Printf.printf "trovato un \"(\"\n";
                      Stack.push "(" operators                          
                    | ")" -> pop_until_parenthesis operators output                                             
                    | ("+" | "-" | "*" | "/" | "^") -> 
                                                    Printf.printf "trovato un operatore %s\n" x;
                                                    (*ops operators output x  *)                     
                    | x -> 
                      Printf.printf "trovato un numero %s\n" x;
                      Stack.push x output  (*nel caso sia un numero lo metto nello stack di output*)                          
          )                                                              
          (String.split_on_char ' ' infix)
  in 
                                            
  (*svuota_op operators output;*)                                                              
                                                                
  String.concat " " (stack_to_list output)
;;                                                                                     
                                                                                        
                                                                      
                                                                        
(*testing*)                             
let espressione = shunting_yard  "3 + 4 * 5" ;;