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
    Printf.printf "controllo associativita': %b\n" check_associativity ;                                               
    Printf.printf "controllo identita': %b\n" check_identity ;                                                                                                                                                                                                  
    check_associativity && check_identity                                                                                                                                                                                   
                                                                                                                                                                                                                            
end                                                                                                                                                                                                                         
(*-------------------------------------------------- TESTING MONOIDI -----------------------------------------------*)                                                                                                                                                                                                                       
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
                                                                                                                 
module IntMonoid = Monoid(struct (* esempio 2 monoide sugli elementi interi *)             
   type t = int                                                                            
   let elems = [0;1;2;3;4;5;6;7;8;9;10]                                                    
   let op a b = (a + b) mod 11                                                             
   let identity = 0                                                                        
end)                                                                                                                   
                                                                                                                       
let () =                                                                                                                                                                                                                    
  let test_monoid =                                                                                                                                                                                                         
    IntMonoid.is_monoid                                                                                                                                                                                                    
  in                                                                                                                                                                                                                        
    Printf.printf "IntMonoid: %b\n" test_monoid                                                                                                                                                                            
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
    let check_invertibility =                                          (* per ogni elemento *)                                                                                                                                
      List.for_all(fun y ->                                                                                                                                                                                                   
        List.exists(fun x ->                                        (* per ogni elemento esiste un elemento che *)                                                                                                                                                                                                   
          Struct.op x y = Struct.identity &&    (* per ogni elemento della struttura *)                                                                                                                                                         
          Struct.op y x = Struct.identity                                                                                                                                                                                     
          ) Struct.elems               (* x (op) y = identita' &&  y (op) x = identita'*)                                                                                                                                     
        ) Struct.elems                                                                                                                                                                                                       
                                                                                                                                                                                                                              
    let is_group =                                                                                                                                                                                                            
      Printf.printf "controllo chiusura : %b\n" check_closure ;                                                                                                                                                               
      Printf.printf "controllo associativita': %b\n" check_associativity ;                                                                                                                                                    
      Printf.printf "controllo identita': %b\n" check_identity ;                                                                                                                                                              
      Printf.printf "controllo invertibilita': %b\n" check_invertibility ;                                                                                                                                                                                                  
      check_closure && check_associativity && check_identity && check_invertibility                                                                                                                                                                                
                                                                                                                                                                                                                              
  end                                                                                                                                                                                                                         
                                                                                                                                                                                                                              
module BoolGroup = Group (struct  (* esempio 3 gruppo sugli elementi booleani NON E' UN GRUPPO !!! *)                                                                                                                                             
  type t = bool                                                                                                                                                                                                               
  let elems = [true; false]                                                                                                                                                                                                   
  let op a b = a || b                                                                                                                                                                                                         
  let identity = false                                                                                                                                                                                                        
end)                                                                                                                                                                                                                          
                                                                                                                                                                                                                              
let () =                                                                                                                                                                                                                      
  let test_group =                                                                                                                                                                                                            
    BoolGroup.is_group                                                                                                                                                                                                        
  in                                                                                                                                                                                                                          
    Printf.printf "IntMonoid: %b\n" test_group                                                                                                                                                                                
;;                                                                                                                                                                                                                          
                                                                                                                                                                                                                            
module IntGroup = Group (struct  (* esempio 4 gruppo sui numeri interi (utilizzare i thunks su gli elementi)*)                                                                                                                                             
  type t = int                                                                                                                                                                                                              
  let elems = [0;1;2;3;4;45;6;7;8;9;10;11;12;13]                                                                                                                                                                                                   
  let op a b = a + b                                                                                                                                                                                                         
  let identity = 0                                                                                                                                                                                                          
end)                                                                                                                                                                                                                          
                                                                                                                                                                                                                                     
let () =                                                                                                                                                                                                                             
  let test_group =                                                                                                                                                                                                                   
    IntGroup.is_group                                                                                                                                                                                                                
  in                                                                                                                                                                                                                                 
    Printf.printf "IntMonoid: %b\n" test_group                                                                                                                                                                                       
;;                                                                                                                                                                                                                                   
                                                                                                                                                                                                                                     
module Z4Group = Group (struct  (* esempio 5 gruppo su Z4 interi *)                                                                                                                                             
  type t = int                                                                                                                                                                                                                       
  let elems = [0;1;2;3]                                                                                                                                                                                                              
  let op a b = (a + b) mod 4                                                                                                                                                                                                         
  let identity = 0                                                                                                                                                                                                                   
end)                                                                                                                                                                                                                                 
                                                                                                                                                                                                                                     
let () =                                                                                                                                                                                                                             
  let test_group =                                                                                                                                                                                                                   
    Z4Group.is_group                                                                                                                                                                                                                 
  in                                                                                                                                                                                                                                 
    Printf.printf "IntMonoid: %b\n" test_group                                                                                                                                                                                       
;;                                                                                            
                                                                                              
module AGroup = Group (struct  (* esempio 6 gruppo su a* (utilizzare i thunks su gli elementi)*)                                                                                                                                             
  type t = string                                                                                                                                                                                                                       
  let elems = ["a"]                                                                                                                                                                                                              
  let op a b = a ^ b                                                                                                                                                                                                         
  let identity = ""                                                                                                                                                                                                                   
end)                                                                                                                                                                                                                                 
                                                                                                                                                                                                                                     
let () =                                                                                                                                                                                                                             
  let test_group =                                                                                                                                                                                                                   
    AGroup.is_group                                                                                                                                                                                                                 
  in                                                                                                                                                                                                                                 
    Printf.printf "IntMonoid: %b\n" test_group                                                                                                                                                                                       
;;                                                                                            
                                                                                                                                                                                                                                     
                                                                                                                                                                                                                          
(* module Ring : AlgebraicStructure = struct                                                                                                                                                                              
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
                                                            
let l = [true; false] ;;                                     
let op a b = a || b ;;                                       
let id = false ;;                                            
                                                             
let check_closure =                                                                              
  List.for_all(fun a ->                                                               
                    List.for_all(fun b ->                                             
                                  List.mem (op a b) l        
                                ) l                          
                ) l                                          
;;                                                           