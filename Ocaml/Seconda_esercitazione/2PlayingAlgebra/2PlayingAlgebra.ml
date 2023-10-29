(** equivalente della classe astratta di java per una struttura albebrica che definisce:                             
    t     : tipo degli elementi trattati dalla struttura algebrica                                                   
    elems : lista degli elementi della struttura algebrica                                                           
    op    : operazione definita per la struttura algebrica                                                           
    identity: elemento identita' utilizzato dalla struttura                                                          
    *)                                                                                                               
module type AlgebraicStructure = sig                                                                                 
  type t                                                                                                             
  val elems : t list                                                                                                 
  val op : t -> t -> t                                                                                               
  val identity : t                                                                                                     
end                                                                                                                  
                                                                                                                     
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
    Printf.printf "controllo associativita': %b\n" check_associativity ;                                                                                                                                                    
    Printf.printf "controllo identita': %b\n" check_identity ;                                                                                                                                                                                                  
    check_associativity && check_identity                                                                                                                                                                                   
                                                                                                                                                                                                                            
end                                                                                                                                                                                                                         
(*-------------------------------------------------- TESTING MONOIDI -----------------------------------------------*)                                                                                                                                                                                                                       
(* definizione delle varie casistiche di test date dall'esercizio *)                                                                                                                                                        
module BoolMonoid : AlgebraicStructure = struct (* esempio 1 monoide sugli elementi booleani *)                                                                                                                                                          
  type t = bool                                                                                                                                                                                                             
  let elems = [false; true]                                                                                                                                                                                                 
  let op a b = a || b                                                                                                                                                                                                       
  let identity = false                                                                                                                                                                                                                              
end                                                                                                                                                                                                                 
                                                                                                                                                                                                                            
module IntMonoid : AlgebraicStructure = struct (* esempio 2 monoide sugli elementi interi *)                                                                                                                                              
   type t = int                                                                                                                                                                                                             
   let elems = [0;1;2;3;4;5;6;7;8;9;10]                                                                                                                                                                                     
   let op a b = (a + b) mod 11                                                                                                                                                                                              
   let identity = 0                                                                                                                                                                                                         
end                                                                                                                                                                                    
                                                                                    
let () =                                                                            
  (*bisogna prima creare una istanza del modulo dato che ocaml non consente di fare:
    (Monoid (BoolMonoid)).is_monoid *)                                                              
  let module MyMonoid = Monoid (BoolMonoid) in                                          
  Printf.printf "Is Monoid: %b\n" MyMonoid.is_monoid ;
  let module MyMonoid = Monoid (IntMonoid) in                                          
  Printf.printf "Is Monoid: %b\n" MyMonoid.is_monoid                                     
;;                        
                          
                                                                                                                                                                                                                            
(*------------------------------------------------------ GRUPPI -----------------------------------------------------*)                                                                                                     
(* modulo per i gruppi, implementato utilizzando un functor                                                                                                                                                                 
  di fatti prende una struttura algebrica come argomento.                                                                                 
  devono rispettare le proprieta':                                                                                                        
  - associativita'                                                                                                    
  - identita'                                                                                                         
  - invertibilita'                                                                                                       
  *)                                                                                                                  
module Group (Struct : AlgebraicStructure) = struct                                                
  let check_closure =                                                                              
    List.for_all(fun a ->                                                               
                      List.for_all(fun b ->                                             
                                    List.mem (Struct.op a b) Struct.elems  
                                    ) Struct.elems                         
                  ) Struct.elems                                               
                                                                                                    
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
                                                                                                                                                                                                                            
  (* invertibilita' per ogni elemento *)                                                                                                                                                                                    
  let check_invertibility =                                                                                                                                                                          
    List.for_all(fun y ->                                       (* per ogni elemento x *)                                                                                                                                                          
    List.exists(fun x ->                                        (* per ogni elemento y esiste un elemento che *)                                                                                                                                                                                                   
      Struct.op x y = Struct.identity &&                        (* x (op) y = identita' &&  y (op) x = identita'*)                                                                                                                                                         
        Struct.op y x = Struct.identity                                                                                                                                                                                     
        ) Struct.elems                                                                                                                                                   
      ) Struct.elems                                                                                                                                                                                                       
                                                                                                                                                                                                                            
  let is_group =                                                                                                                                                                                                            
    Printf.printf "controllo chiusura : %b\n" check_closure ;                                                                                                                                                               
    Printf.printf "controllo associativita': %b\n" check_associativity ;                                                                                                                                                    
    Printf.printf "controllo identita': %b\n" check_identity ;                                                                                                                                                              
    Printf.printf "controllo invertibilita': %b\n" check_invertibility ;                                                                                                                                                                                                  
    check_closure && check_associativity && check_identity && check_invertibility                                                                                                                                                                                
                                                                                                                                                                                                                            
end                                                                                                                                                                                                                         
                                                                                                                       
(*-------------------------------------------------- TESTING GRUPPI -----------------------------------------------*)
module BoolGroup : AlgebraicStructure = struct  (* esempio 3 gruppo sugli elementi booleani NON E' UN GRUPPO !!! *)                                                                                                                                             
  type t = bool                                                                                                                                                                                                               
  let elems = [true; false]                                                                                                                                                                                                   
  let op a b = a || b                                                                                                                                                                                                         
  let identity = false                                                                                                                                                                                                        
end                                                                                                                                                                                     
                                                                                                                                                                                                                            
module IntGroup : AlgebraicStructure = struct  (* esempio 4 gruppo sui numeri interi (utilizzare i thunks su gli elementi)*)                                                                                                                                             
  type t = int                                                                                                                                                                                                              
  let elems = [0;1;2;3;4;45;6;7;8;9;10;11;12;13]                                                                                                                                                                                                   
  let op a b = a + b                                                                                                                                                                                                         
  let identity = 0                                                                                                                                                                                                          
end                                                                                                                                                                                                                                  
                                                                                                                                                                                                                                     
module Z4Group : AlgebraicStructure = struct  (* esempio 5 gruppo su Z4 interi *)                                                                                                                                             
  type t = int                                                                                                                                                                                                                       
  let elems = [0;1;2;3]                                                                                                                                                                                                              
  let op a b = (a + b) mod 4                                                                                                                                                                                                         
  let identity = 0                                                                                                                                                                                                                   
end                                                                                                                                                                                          
                                                                                                                                                                                                                             
module AGroup : AlgebraicStructure = struct  (* esempio 6 gruppo su a* (utilizzare i thunks su gli elementi)*)                                                                                                                                             
  type t = string                                                                                                                                                                                                                       
  let elems = ["a"]                                                                                                                                                                                                              
  let op a b = a ^ b                                                                                                                                                                                                         
  let identity = ""                                                                                                                                                                                                                   
end                                                                                                                                                                                                                                 
                                                                                                                                                                                                    
let () =                                                            
    let module InstanceBoolGroup = Group (BoolGroup) in                                                                   
    Printf.printf "Is Group: %b\n" InstanceBoolGroup.is_group ;
                                                               
    let module InstanceIntGroup = Group (IntGroup) in                                                                   
    Printf.printf "Is Group: %b\n" InstanceIntGroup.is_group ;
                                                               
    let module InstanceZ4Group = Group (Z4Group) in                                                                   
    Printf.printf "Is Group: %b\n" InstanceZ4Group.is_group ;
                                                               
    let module InstanceAGroup = Group (AGroup) in                                                                   
    Printf.printf "Is Group: %b\n" InstanceAGroup.is_group ;                                                                                                                                                                                      
;;                                                                                                                                                                                                                           
                                                                                                                                                                                                                                     
                                                                                                                                                                                                                             
                                                                                                       
(*------------------------------------------------------ ANELLI -----------------------------------------------------*) 
(* modulo per gli anelli, implementato utilizzando un functor                                                           
  di fatti prende una struttura algebrica come argomento.                                                             
  devono rispettare le proprieta':                                                                                    
  - chiusura su tutte e due le operazioni                                                                                                    
  - associativita' delle due operazioni                                                                                                         
  - commutativita' della prima operazione                                                
  - esistenza dell'elemento identita' della prima operazione                             
  - esistenza dell'inverso della prima operazione                                        
  - proprieta' distributiva                                                              
  - esistenza dell'elemento identita' per la seconda operazione                                                                                                      
  *)                                                                                                   
                                                                                         
(* ridefinizione di struttura algebrica per poter ospitare la seconda operazione prevista dagli anelli *)                                                       
module type AlgebraicStructure = sig                                                                   
  type t                                                                                               
  val elems : t list                                                                                   
  val op : t -> t -> t                                                                                 
  val ring_op: t -> t -> t                                                                             
  val identity : t                                                                                                     
end                                                                                                    
                                                                                                       
module Ring (Struct : AlgebraicStructure) = struct                                       
  let check_commutativity =                                                              
    List.for_all (fun x ->                                           
                    List.for_all (fun y ->                           
                                    (Struct.op x y) = (Struct.op y x)
                                    ) Struct.elems                             
                    ) Struct.elems                
                                                                  
  let check_closure =                                                
    List.for_all(fun a ->                                                               
      List.for_all(fun b ->                                             
                    List.mem (Struct.ring_op a b) Struct.elems       
                    ) Struct.elems                                   
                  ) Struct.elems                                     
  &&                                                                                            
    List.for_all(fun a ->                                                                       
                      List.for_all(fun b ->                                                     
                                    List.mem (Struct.op a b) Struct.elems                       
                                    ) Struct.elems                                              
                  ) Struct.elems                                                                
                                                                                                    
  let check_associativity =                                                                                          
    let rec aux elems =                                                                                            
      match elems with                                                                                             
      | a :: b :: c :: rest ->                                                                                                                                                                                            
        Struct.op (Struct.op a b) c = Struct.op a (Struct.op b c) &&                          
        Struct.op (Struct.op a b) c = Struct.op a (Struct.ring_op b c) &&                                                                                                                                                    
        aux (b :: c :: rest)                                                                                                                                                                                              
      | a :: b :: rest ->                                                                                                                                                                                                 
        Struct.op (Struct.op a b) a = Struct.op a (Struct.op a b) &&                          
        Struct.op (Struct.op a b) a = Struct.op a (Struct.ring_op a b) &&                                                                                                                                                      
        aux (b :: rest)                                                                                                                                                                                                   
      | _ -> true                                                                                                                                                                                                         
  in                                                                                                                                                                                                                      
  aux Struct.elems                                                                                                                                                                                                        
                                                                                                                                                                                                                            
  let check_identity =                                                                                                                                                                                                      
    List.for_all (fun a ->                                                                                           
                    (Struct.op a Struct.identity) = a && (Struct.op Struct.identity a) = a                           
                    ) Struct.elems                                                                                                            
                                                                                                                                                                                                                            
  (* invertibilita' per ogni elemento *)                                                                                                                                                                                    
  let check_invertibility =                                                                                                                                                                          
    List.for_all(fun y ->                                         (* per ogni elemento x *)                                                                                                                                                          
      List.exists(fun x ->                                        (* per ogni elemento y esiste un elemento che *)                                                                                                                                                                                                   
        Struct.op x y = Struct.identity &&                        (* x (op) y = identita' &&  y (op) x = identita'*)                                                                                                                                                         
        Struct.op y x = Struct.identity                                                                                                                                                                                      
        ) Struct.elems                                                                                                                                                   
      ) Struct.elems                                                                               
                                                                                                   
  let check_distribution =                                                                         
    let rec aux elems =                                                                                            
      match elems with                                                                                             
      | a :: b :: c :: rest ->                                                                                                                                                                                            
        Struct.ring_op a (Struct.op b c) = Struct.op (Struct.ring_op a b) (Struct.ring_op a c) &&  
        Struct.ring_op (Struct.op b c) a = Struct.op (Struct.ring_op b a) (Struct.ring_op c a) &&                                                                                                                                                       
        aux (b :: c :: rest)                                                                                                                                                                                              
      | _ -> true                                                                                                                                                                                                         
  in                                                                                                                                                                                                                      
  aux Struct.elems                                                                                 
                                                                                                                                                                                                                            
  let is_ring =                                                                                                                                                                                                            
    Printf.printf "controllo commutativita' : %b\n" check_commutativity ;
    Printf.printf "controllo chiusura : %b\n" check_closure ;                                                                                                                                                               
    Printf.printf "controllo associativita': %b\n" check_associativity ;                                                                                                                                                    
    Printf.printf "controllo identita': %b\n" check_identity ;                                                                                                                                                              
    Printf.printf "controllo invertibilita': %b\n" check_invertibility ;                          
    Printf.printf "controllo distributivita' : %b\n" check_distribution ;                                                                                                                                                                                                  
    check_commutativity &&                                                                                                                                                                                                
    check_closure &&                                                                                                                                                                                                      
    check_associativity &&                                                                                                                                                                                                
    check_identity &&                                                                                                                                                                                                     
    check_invertibility &&                                                                                                                                                                                                
    check_distribution                                                                                                                                                                                                    
                                                                                                                                                                                                                            
end                                                                                                                                                                                                                                           
                                                                                                                      
(*-------------------------------------------------- TESTING ANELLI -----------------------------------------------*)
module Zero : AlgebraicStructure = struct      (* esempio 7 anello su {0} *)                                                                                                                                                                                           
  type t = int                                                                                                                                                                                                                               
  let elems = [0]                                                                                                                                                                                                                            
  let op a b = 0                                                                                                                                                                                                                             
  let ring_op a b = 0                                                                                                                                                                                                                        
  let identity = 0                                                                                                                                                                                                                           
end                                                                                                                                                                                                                                                                                                                                                                                                                                                    
                                                                                                                                                                                                                                             
                                                                                                                                                                                                                                             
module IntRing : AlgebraicStructure = struct  (* esempio 8 anello su Int (utilizzare i thunks su gli elementi)*)                                                                                                                                                                                                    
type t = int                                                                                                                                                                                                                                 
let elems = [0;1;2;3;4;5;6;7;8;9;10]                                                                                                                                                                                                         
let op a b = a + b                                                                                                                                                                                                                           
let ring_op a b = a * b                                                                                                                                                                                                                      
let identity = 0                                                                                                                                                                                                                             
end                                                                                                                                                                                                        
                                                                                                                                                                                                                                             
                                                                                 
                                                                                 
module Z4Ring : AlgebraicStructure = struct  (* esempio 9 anello su Z4 *)                                                                                                                                                                                                    
type t = int                                                                                                                                                                                                                                       
let elems = [0;1;2;3]                                                                                                                                                                                                                              
let op a b = (a + b) mod 4                                                                                                                                                                                                                           
let ring_op a b = (a * b) mod 4                                                                                                                                                                                                                      
let identity = 0                                                                                                                                                                                                                                   
end                                                                                                                                                                                                              
                                                                                                                                                                                                                                                   
let () =                                                                                                                                                                                                                                           
    let module InstanceZero = Ring (Zero) in                                                                                                                                                                                                                                           
    Printf.printf "Is Ring: %b\n" InstanceZero.is_ring ;                                                                                                                                                                                           
                                                                                                                                                                                                                                                   
    let module InstanceZ4Ring = Ring (Z4Ring) in                                                                                                                                                                                                   
    Printf.printf "Is Ring: %b\n" InstanceZ4Ring.is_ring ;                                                                                                                                                                                         
                                                                                                                                                                                                                                                   
    let module InstanceIntRing = Ring (IntRing) in                                                                                                                                                                                                        
    Printf.printf "Is Ring: %b\n" InstanceIntRing.is_ring ;                                                                                                                                                                                                  
;;                                                                                                                                                                                                                                                 