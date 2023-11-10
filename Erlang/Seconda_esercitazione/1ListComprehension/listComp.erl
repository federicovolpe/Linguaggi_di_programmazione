- module (listComp).                                                                    
- export ([main/0]).                                                                    
                                                                                                                   
main() ->                                                                                                          
    Tests = [[1, 2, 12, "34", "ciso", 0, telegrafo],                                                              
             ["ciao", 6, "ciso", 0, 2, rimpianto],                                                                
             [],                                                                                                  
             ["ciao", 6, "ciso", 0, 2, rimpianto,["ciao", 6, "ciso", 0, 2, rimpianto]]],                          
                                                                                                                  
    lists:map(fun (List) -> % eventuale combinazione delle liste per i test...                                                                                 
                io:format("squared_int di: ~n~p ---> ~n~p~n",[List, squared_int(List)]),                                                                                                
                io:format("intersect di: ~n~p e ~n~p ---> ~n~p~n",[List, List, intersect(List, List)]),                                                                                               
                io:format("symmetric_difference di: ~n~p e ~n~p ---> ~n~p~n~n",[List, List, differenza_simmetrica(List, List)])                                                               
              end, Tests).                                                                                        
                                                                                                                  
                                                                                                                   
%---------------------------------------------- squared_int -----------------------------------------------------  
                                                                                                                   
squared_int(L) -> % funzione che presa una lista ritorna la lista ma con solo i numeri quadrati                    
    square(filter_numbers(L)).                                                                                    
                                                                                                                  
square([]) -> % caso lista finita ritorno niente                                                                   
    [];                                                                                                            
square([H| T]) -> [math:pow(H, 2) | square(T)]. %caso lista piena calcolo la potenza del primo                     
                                                                                                                                                                                                                                    
filter_numbers(L) -> % funzione che filtra solo i numeri della lista                                                  
    lists:filter(fun(X) -> is_number(X) end, L).                                                                   
                                                                                                                                                           
%---------------------------------------------- interserct -----------------------------------------------------                                           
                                                                                                                                                           
intersect(A, B) -> % funzione che prese due liste ritorna l'intersezione delle due                                                                         
    lists:filter(fun(X) -> lists:member(X, B) end, A).     % elementi di a che siano presenti anche in b                                                                                                     
                                                                                                                                                           
%---------------------------------------------- symmetric_difference -----------------------------------------------------                                 
                                                                                                                                                           
differenza_simmetrica(A, B) -> % unisce le liste togliendo gli elementi in comune                                                                                                                            
    lists:filter(fun(X) -> not lists:member(X, B) end, A)  % tutte quelle contenute in b ma non in a 
    ++                                                                                                                                                       
    lists:filter(fun(X) -> not lists:member(X, A) end, B). % tutte quelle in a ma non in b                                                                                                