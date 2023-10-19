module type ALGEBRAIC_SORT = sig                                                                              
  type t                                                                                                      
  val equal : t -> t -> bool                                                                                  
  val to_string : t -> string                                                                                 
end                                                                                                           
                                                                                                              
module Monoid (S : ALGEBRAIC_SORT) = struct                                                                   
  let check_monoid add i =                                                                                    
    (* Check associativity *)                                                                                 
    let associativity a b c = add a (add b c) = add (add a b) c in                                            
                                                                                                              
    (* Check identity element *)                                                                              
    let identity_element a = (add a i = a) && (add i a = a) in                       
                                                                                     
    (* Return true if it's a monoid, false otherwise *)                              
    associativity S.to_string S.to_string S.to_string && identity_element S.to_string
end                                                                                  
                                                                                     
(*module Group (S : ALGEBRAIC_SORT) = struct                                                           
  let check_group add i =                                                                              
    (* Check associativity *)                                                                          
    let associativity a b c = add a (add b c) = add (add a b) c in                                     
                                                                                                       
    (* Check identity element *)                                                                       
    let identity_element a = (add a i = a) && (add i a = a) in                                         
                                                                                                       
    (* Check invertibility *)                                                                          
    let invertibility a = (add a (add (S.to_string a) (i a)) = i a) && (add (S.to_string a) a = i a) in
                                                                                                       
    (* Return true if it's a group, false otherwise *)                                                 
    associativity S.to_string S.to_string S.to_string && identity_element S.to_string && invertibility S.to_string
end                                                                                                    
                                                                                                       
module Ring (S : ALGEBRAIC_SORT) = struct                                                              
  let check_ring add mul i =                                                                           
    (* Check addition is an abelian (commutative) group *)                                             
    let is_abelian_group = Group(S).check_group add i in                                               
                                                                                                       
    (* Check multiplication distributes over addition *)
    let distributivity a b c = mul a (add b c) = add (mul a b) (mul a c) && mul (add b c) a = add (mul b a) (mul c a) in
                                    
    (* Return true if it's a ring, false otherwise *)
    is_abelian_group && distributivity S.to_string S.to_string S.to_string
end                                 
                                    
 Test with Monoids, Groups, and Rings *)
module BoolMonoid = Monoid (struct  
  type t = bool                     
  let equal = (=)                   
  let to_string b = string_of_bool b
end)                                
                                    
(* module IntMonoid = Monoid (struct   
  type t = int                      
  let equal = (=)                   
  let to_string n = string_of_int n 
end)                                
                                    
module BoolGroup = Group (struct    
  type t = bool                     
  let equal = (=)   
  let to_string b = string_of_bool b
end)             
                 
module IntGroup = Group (struct
  type t = int
  let equal = (=)
  let to_string n = string_of_int n
end)

module BoolRing = Ring (struct
  type t = bool
  let equal = (=)
  let to_string b = string_of_bool b
end)         
                                                
module IntRing = Ring (struct                   
  type t = int                                  
  let equal = (=)                               
  let to_string n = string_of_int n             
end)                                            
                                                
Tests *)                                     
let () =                                        
  let test_monoid = BoolMonoid.check_monoid (||) false in
  Printf.printf "BoolMonoid: %b\n" test_monoid; 
                                                
  let test_monoid = IntMonoid.check_monoid (+) 0 in
  Printf.printf "IntMonoid: %b\n" test_monoid;  
                                                
  let test_group = BoolGroup.check_group (||) false in
  Printf.printf "BoolGroup: %b\n" test_group;   
                                                
  let test_group = IntGroup.check_group (+) 0 in
  Printf.printf "IntGroup: %b\n" test_group;    
                                               
  let test_ring = BoolRing.check_ring (||) (&&) false in
  Printf.printf "BoolRing: %b\n" test_ring;

  let test_ring = IntRing.check_ring (+) ( * ) 0 in
  Printf.printf "IntRing: %b\n" test_ring;
