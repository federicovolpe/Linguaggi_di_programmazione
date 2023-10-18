(** equivalente della classe astratta di java per una struttura albebrica che definisce:            
    t     : tipo degli elementi trattati dalla struttura algebrica                                  
    elems : lista degli elementi della struttura algebrica                                          
    op    : operazione definita per il monoide allo stato attuale della risoluzione dell'esercizio 
    identity: elemento identita' utilizzato dalla struttura       
    *)                                                            
module type AlgebraicStructure = sig                                                                
  type t                                                                                            
  val elems : t list                                                                                
  val op : t -> t -> t                                                                              
  val identity : t                                                                                                     
end                                                                                                             
                                                                                                                
(** un modulo per ogni struttura algebrica come da consegna *)                                                  
                                                                                                                
(* modulo per i monoidi, implementato utilizzando un functor
  di fatti prende una struttura algebrica come argomento.
  devono rispettare le proprieta':                       
  - associativita'                                                       
  - identita'                                                           
  *)                                                                                                           
module Monoid (Struct : AlgebraicStructure) = struct                                                                 
  let check_associativity =                                                                                     
      let rec aux elems =                                                                                       
        match elems with                                                                                        
        | a :: b :: c :: rest ->                                                                                                                                  
          Struct.op (Struct.op a b) c = Struct.op a (Struct.op b c) &&                                          
          aux (b :: c :: rest)                                                                 
        | a :: b :: rest ->                                                                                                                                                                                           
          Struct.op (Struct.op a b) a = Struct.op a (Struct.op a b) &&                                                                                                                                                
          aux (b :: rest)                                                                                                                                                                                             
        | _ -> true                                                                                                                                                                                                   
    in                                                                                                                                                                                                                
    aux Struct.elems                                                                                                                                                                                                  
                                                                                                                                                                                                                      
  let check_identity =                                                                                                                                                                                                
    List.for_all (fun a -> (Struct.op a Struct.identity) = a && (Struct.op Struct.identity a) = a ) Struct.elems                                                                                                      
                                                                                                                                                                                                                      
  let is_monoid =                                                                                                                                                                                                           
    check_associativity && check_identity                                                                                                                                                                                   
                                                                                                                                                                                                                            
end                                                                                                                                                                                                                         
                                                                                                                                                                                                                            
(* definizione delle varie casistiche di test date dall'esercizio *)                                                                                                                                                        
module BoolMonoid = Monoid (struct (* esempio 1 monoide sugli elementi booleani *)                                                                                                                                                          
  type t = bool                                                                                                                                                                                                             
  let elems = [false; true]                                                                                                                                                                                                 
  let op a b = a || b                                                                                                                                                                                                       
  let identity = false                                                                                                                                                                                                                              
end                                                                                                                                                                                                                         
)                                                                                                                                                                                                                           
                                                                                                                                                                                                                            
let () =                                                                                                                                                                                                                    
  let test_monoid =                                                                                                                                                                                                         
    BoolMonoid.is_monoid                                                                                                                                                                                                    
  in                                                                                                                                                                                                                        
    Printf.printf "BoolMonoid: %b\n" test_monoid                                                                                                                                                                            
;;                                                                                                                                                                                                                          
                                                                                              
(*                                                                                                     
module Group : AlgebraicStructure = struct                                                                              
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
                                                               
                                                               
module Ring : AlgebraicStructure = struct                
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
                                             
module Set  = struct                
  type t = T list                              
                                               
  let empty = []              
                                              
  let insert (x: T) (s: t) = x :: s            
                                               
  let contains (x: T) (s: t) = List.mem x s    
                                               
  let remove (x: T) (s: t) =                   
    List.filter (fun y -> y <> x) s            
                                               
end                           
  
*)