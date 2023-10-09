(** modulo interfaccia per l'implementazione delle operazioni necessarie alle strutture algebriche   
module type AlgebraicStructure = sig                                                                 
  val add : 'a -> 'a -> 'a                                                                           
  val mult : 'a -> 'a -> 'a                                                                          
  val identity : 'a -> 'a -> 'a                                                                      
end *)                                                                                                 
                                                                                                     
module Monoid = struct                                                                               
                                                                                                     
  let add : 'a -> 'a -> 'a                                                                           
  let mult : 'a -> 'a -> 'a                                                                          
  let identity : 'a                                                                                  
  let is_monoid: bool =                                                                              
  let check_closure = ∀ (x: T) (y: T), add x y ∈ T in                                                
  let check_associativity = ∀ (x: T) (y: T) (z: T), add (add x y) z = add x (add y z) in             
  let check_identity = ∀ (x: T), add i x = x ∧ add x i = x in                                        
  check_closure && check_associativity && check_identity                                             
                                                                                                     
end                                                                                                  
                                                                                                     
(*module Group = struct                                                                              
   additive operation                                
  let add                                                
  ;;                                                     
  (* multiplicative operation *)                         
  let mul                                                
  ;;                                                     
  (* identity operand *)                                 
  let i                   
  ;;                      
                          
end                     
                          
module Ring = struct    
  (* additive operation *)
  let add                 
  ;;                      
  (* multiplicative operation *)           
  let mul                                  
  ;;                                       
  (* identity operand *)                   
  let i                                    
  ;;                                       
                                           
end                                          
                                             
module Set (T: Type) = struct                
  type t = T list                              
                                               
  let empty = []             
                                              
  let insert (x: T) (s: t) = x :: s            
                                               
  let contains (x: T) (s: t) = List.mem x s    
                                               
  let remove (x: T) (s: t) =                   
    List.filter (fun y -> y <> x) s            
                                               
end                                            
                                               
let x = ∀ x ;;                         
                    *)