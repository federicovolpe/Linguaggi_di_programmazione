(** -------------------------- is_palindrome --------------------------*)                                                                               
let rec reverse (str:string): string =                                                                                                 
  match str with                                                                                                                       
  | "" -> ""                                                                                                                           
  | s -> (String.sub s (String.length s - 1) 1) (*prende l'ultimo carattere*)                                                          
      ^ reverse (String.sub s 0 (String.length s - 1)) (*funzione sulla sottostringa restante*)                                                                               
;;                                                                                                                                                                                                                                                                          
                                                                                                                                       
let lowerstring (s:string): string =                                                                                                   
  String.lowercase_ascii s                                                                                                             
;;                                                                                                                                     
                                                                                                                                       
let removespecialchar (s:string): string =                                                                                             
  (*concatenazione della stringa spezzata in corrispondenza del carattere...*)                                                                                           
  let res = String.concat "" (String.split_on_char ' ' s) in                                                                           
  let res = String.concat "" (String.split_on_char ',' res) in                                                                         
  let res = String.concat "" (String.split_on_char '.' res) in                                                                         
  let res = String.concat "" (String.split_on_char '!' res) in                                                                         
  String.concat "" (String.split_on_char '?' res)                                                                                      
;;                                                                                                                                     
                                                                                                                                       
let is_palindrome (s : string) : bool =                                                                                                
  (* stringa originale lowered e senza caratteri speciali *)                                                                                                                 
  let og = lowerstring (removespecialchar s) in                                                                                        
  (* stringa precedente al contrario *)                                                                          
  let rev = reverse og in                                                                                        
                                                                                                                 
  print_string rev ;                                                                                             
  print_string og ;                                                                                              
  String.equal rev og                                                                                       
;;                                                                                  
                                                                                    
(** -------------------------- (-) --------------------------*)                     
                                                                                    
(**caso in cui si volesse sottrarre una sola volta i caratteri della seconda stringa*)
let rec subStringUnary (l1: char list) (l2: char list): char list =                      
    let rec subchar (l1: char list) (c: char): char list =                                        
      match l1 with                                                                 
      | h :: tail when h = c -> tail                                                
      | h :: tail -> h :: subchar tail c                                                             
      | [] -> []                                                                                                   
    in                                                                                                             
  match l2 with                                                                                                          
  | h :: tail when List.mem h l1 -> subStringUnary (subchar l1 h) tail                                                   
  | h :: tail -> subStringUnary l1 tail                                                                                  
  | [] -> l1                                                                                                             
;;                                                                                                                       
                                                                                                                         
(**caso in cui si vogliono togliere tutte le ricorrenze dei caratteri della stringa 2 dalla stringa 1*)                  
let rec subString (l1: char list) (l2: char list): char list =                                                                                                                    
  match l1 with                                                                                                                                   
  | h :: tail when List.mem h l2 -> (* se la testa e' contenuta nella seconda lista processo la coda *)                                           
                                    subString tail l2                                                                                             
  | h :: tail -> h :: subString tail l2                                                                                                           
  | [] -> []                                                                                                                                      
;;                                                                                                                                                
                                                                                                                                                  
let compose_string (l: char list): string =                                                                                                         
    let rec aux (list: char list) (res: string) =                                                                                                                                      
      match list with                                                                                                                                                                  
      | [] ->                                                                                                                                     
              res                                                                                                                                                                        
      | h :: t ->                                                                                                                                 
              (* da carattere a stringa e lo concateno al risultato *)                                                                            
              aux t (res ^ String.make 1 h)                                                                                                                                           
  in                                                                                                                                                                                   
  aux l ""                                                                                                                                                                             
;;                                                                                                                                                                                     
                                                                                                                                                                                       
let (-) (s1: string) (s2: string) : string =                                                                                                                                                                                                           
  (*decomposizione delle stringhe in liste su cui fare pattern matching facilmente*)                                                                                                                                                                   
  let l1 = String.fold_left (fun acc c -> acc @ [c]) [] s1 in                                                                                                                                                                                          
  let l2 = String.fold_left (fun acc c -> acc @ [c]) [] s2 in                                                                                                                                                                                          
                                                                                                                                                                                                                                                       
  (* composizione della lista risultante *)                                                                                                                                                                                                            
  compose_string (subString l1 l2)                                                                                                                                                                                                                     
;;                                                                                                                                                                                                                                                     
                                                                                                                                                                                                                                                       
(** -------------------------- anagram --------------------------*)              
(** date due stringhe stabilisce se sono anagrammi *)                                
let are_anagrams (s1: string) (s2: string) : bool =                                                                                                 
  let l1 = List.sort                           (*scomposizione e sorting in lista*)                                                                                                    
            (fun c1 c2 -> Char.compare c1 c2)                                                                                                       
            (String.fold_left (fun acc c -> acc @ [c]) [] s1) in                                                                                    
  let l2 = List.sort                                                                                                                                
            (fun c1 c2 -> Char.compare c1 c2)                                                                                                       
            (String.fold_left (fun acc c -> acc @ [c]) [] s2) in                                                                                    
                                                                                                                                                    
  List.equal (fun c1 c2 -> c1 = c2) l1 l2      (*comparazione delle due liste sortate*)                                                                                                       
;;                                                                                                                                                                                                
let rec anagram (s: string) (l: string list) : bool =                                                                                                                                             
  match l with                                                                                                                                                                                    
  | h :: t when are_anagrams h s -> true                                                                                                                                                          
  | h :: t -> anagram s t                                                                                                                                                                         
  | [] -> false                                                                                                                                                                                   
;;                                                                                                                                                                                                
                                                                                                                                                                                                  
(*testing... *)                                                                                                                                                                                                 
anagram "nace" ["ociredef";"cane";"pippo";"pasta";"pluto";"paperino";"qui";"quo";"qua"];;                                                                                                     