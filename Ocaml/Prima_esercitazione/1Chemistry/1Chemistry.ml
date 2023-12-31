(**ercizio agggiunto pari pari a come lo ha svolto il professore
    con la sola aggiunta di commenti per la spiegazione *)
                                                          
let alkaline_earth_metals = ("beryllium", 4)::("magnesium", 12)::("calcium", 20)::("strontium", 38)::("barium", 56)::("radium", 88)::[] ;;
                                                                       
let noble_gases = [("helium", 2); ("neon", 10); ("argon", 18); ("krypton", 36); ("xenon", 54); ("radon", 86)] ;;
                                                          
(** funzione per il sorting delle tuple *)                
let (>:) a b =                                            
    (snd a) - (snd b)                                     
;;                                                        
                                                          
let max a b =            
    if (a >: b >= 0)     
        then a           
    else b               
;;                                            
                                                                       
let heaviest lst =                                                     
  List.fold_left max (List.hd lst) (List.tl lst) ;;                    
                                                                       
let sort_ascending lst =                      
    List.sort (>:) lst                        
;;                                            
                                              
let merge_elements metals gases =             
    (*sorting delle due liste *)            
  let sorted_metals = sort_ascending metals and 
      sorted_gases = sort_ascending gases     
in                                            
List.merge (>:) sorted_metals sorted_gases    
;;                                            
                                              
let print_list f lst =                        
  let rec print_list = function               
    []     -> ()                              
  | hd::[] -> f hd                            
  | hd::tl -> f hd ; print_string ", " ; print_list tl 
in print_string "[" ; print_list lst ; print_string "]\n" ;;
                                            
let print_pair a =                          
  Printf.printf "(%s, %d)" (fst a) (snd a) ;;
                                                   
let main() =                                       
  let m = (heaviest alkaline_earth_metals) and     
      sorted_aem = sort_ascending alkaline_earth_metals in
    Printf.printf "The heaviest element is «%s» with «%2d» as atomic number.\n" (fst m) (snd m) ;
    print_list print_pair sorted_aem ;             
    print_list print_pair (merge_elements alkaline_earth_metals noble_gases) ;;
                                                   
main() ;;                                          
                                                   