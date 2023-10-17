(*Esercizio non ancora terminato !                      
                                                        
L'esercizio viene risolto con l'utilizzo dello Shunting Yard algorithm per la traduzione
   da notazione classica a polish *)                                                                                                                             
                                                                                   
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
  if not (Stack.is_empty operators) (*se lo stack degli operatori non e' vuoto*)
    then     
        (let precedenza_x = precedence (x) in
         let top_operator = precedence (Stack.top operators) in
                                           
        if Stack.is_empty operators && top_operator >= precedenza_x (*se la precedenza del top dello stack è maggiore allora lo tolgo*)                                               
          then                                             
            (Stack.push (Stack.pop operators) output;       
            ops operators output x;)
          else (* se sono arrivato ad un operatore con la stessa precedenza allora aggiungo questo *)
            print_string "lo metto fra gli operatori1\n";
            Stack.push x operators) 
    else (*se lo stack degli operatori e' vuoto allora aggiungo l'operatore e basta*)
      print_string "lo metto fra gli operatori2\n";
      Stack.push x operators;
;;         
                                                     
let rec svuota_op operators output =                 
  if not (Stack.is_empty operators) (*se c'e' qualcosa nello stack degli operatori*)                     
    then                                             
      (Stack.push (Stack.pop operators) output; 
      Printf.printf "svuotato %s\n" (Stack.top output);      
      svuota_op operators output;)  
  else ()
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
    match x with                                                    
                    | "(" -> 
                      Printf.printf "trovato un \"(\"\n";
                      Stack.push "(" operators                          
                    | ")" -> pop_until_parenthesis operators output                                             
                    | ("+" | "-" | "*" | "/" | "^") -> 
                                                    Printf.printf "trovato un operatore %s\n" x;
                                                    ops operators output x ;                      
                    | x -> 
                      Printf.printf "trovato un numero %s\n" x;
                      Stack.push x output  (*nel caso sia un numero lo metto nello stack di output*)                          
          )                                                              
          (String.split_on_char ' ' infix)
  in 
                                            
  svuota_op operators output;                                                             
                                                                
  String.concat " " (stack_to_list output)
;;                                                                                     
                                                                                        
                                                                      
                                                                        
(*testing*)                             
let () = Printf.printf "risultato %s\n" (shunting_yard  "3 + 4 * 5 + 6 - 2") ;;