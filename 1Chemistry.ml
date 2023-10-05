let alkaline_earth_metals = ("beryllium", 4) :: ("magnesium", 12) :: ("calcium", 20) ::                              
                              ("strontium", 38) :: ("barium", 56) :: ("radium", 88) :: ("polonium", 77)::[]  ;;                            
                                                                                                                
let highestAtomicNumber(list) =                                                                                 
    let rec aux(list, highest) =                                                                                
        match list with                                                                                         
        | [] -> highest                                                                                         
        | (metal, n) :: t -> if n > (snd highest) then                                                          
                                aux(t, (metal, n))                                                              
                            else                                                                                
                                aux(t, highest)                                                                                                                                      
    in                                                                                                                                                                               
aux(list, ("", 0)) ;;                                                                                                                                                                
                                                                                                                                                                                     
(** sorting funciton  *)                                                                                                                                                             
let criteria a b = if snd a > snd b then 1                                                                                                                                           
                     else if snd a == snd b then 0                                                                                                                                   
                     else -1;;                                                                                                                                                       
                                                                                                                                                                                     
List.sort (criteria) alkaline_earth_metals;;                                                                
                                                                                                            
let gasses = ("helium", 2)::("neon", 10)::("argon", 18)::("krypton", 36)::("xenon", 54)::("radon", 86)::[];;
                                                                                                         
let sortNome a:('a * 'b) b:('a * 'b) =                                                                                       
    let rec strings x y =                                                                                
        match (x, y) with                                                                                
        | (h1::t1 , h2::t2) -> if criteria h1 h2 == 0 then strings t1 t2                                 
                               else criteria h1 h2                                                       
        | ([], z) -> -1                                                                                  
        | (z, []) -> 1                                                                                         
    in                                                                                                   
    strings (fst a) (fst b);;                                                                            
                                                                                                         
let x = gasses @ alkaline_earth_metals;;                                                                 
                                                                                                         
List.merge sort;;                              
                                                   