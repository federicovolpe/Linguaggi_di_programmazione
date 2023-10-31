-module(sequential).                                                                                                                                                                   
-export([start/0,factors/1,is_prime/1]).                                                                                                                                                             
                                                                                                                                                                                                                                                                                                                           
start() ->                                                                                                                                                                                                                                                                                                                 
                                                                                                                                                                                                                                                                                                                           
    io:format("------------------------------ is_palindrome -------------------------------~n"),                                                                                                                                                                                                                           
    InputTestPalindrome = ["Do geese see God?", "Rise to vote, sir.", "not a palindrome string"],                                                                                                                                                                                                                                    
    lists:map (fun (Stringa) ->                                                                                                                                                                                                                                                                                            
                    io:format("~p -> ~p ~n", [Stringa, is_palindrome(Stringa)]) end,                                                                                                                                                                                                                                       
                    InputTestPalindrome),                                                         
                                                                                                                                                                                                                                                                                                        
    io:format("------------------------------ is_an_anagram -------------------------------~n"),                                                                                                                                                                                                                           
    Dictionary = ["albero", "cane", "gatto", "casa", "sole", "mare", "montagna",                  
                "fiore", "libro", "musica", "lingua", "amico", "famiglia", "giorno",              
                "notte", "tavolo", "chitarra", "cucina", "quadro", "computer", "calcio",             
                "pizza", "scuola", "strada", "telefono", "amore", "viaggio", "vento",                
                "bicicletta", "risata", "federico", "topo", "scoiattolo", "scrivania"],                                                                                                                            
    InputTestAnagram = ["cane","poto","ttoga","ociredef"],                                                                                                                                                                                                                                                                        
    lists:map (fun (Stringa) ->                                                                                                                                                                                                                                                                                            
                    io:format("~p -> ~p ~n", [Stringa, is_an_anagram(Stringa, Dictionary)]) end,                                                                                                                                                                                                                                       
                    InputTestAnagram),                                                            
                                                                                                  
    io:format("--------------------------------- is_prime ----------------------------------~n"),    
    InputTestNumbers = [1,2,3,4,5,6,7,8,9,1000,113,137,999],                                          
    lists:map (fun (Numero) ->                                                                                                                                                                                                                                                                                            
                        io:format("~p -> ~p ~n", [Numero, is_prime(Numero)]) end,                                                                                                                                                                                                                                       
                        InputTestNumbers),                                                                                
                                                                                                                                                                                                                                                                                                                             
    io:format("--------------------------------- factors ----------------------------------~n"),                                
    lists:map (fun (Numero) ->                                                                                                                                                                                                                                                                                            
                        io:format("~p -> ~p ~n", [Numero, factors(Numero)]) end,                                                                                                                                                                                                                                       
                        InputTestNumbers),                                                                                      
                                                                                                                                                                                                                                                                                                                              
    io:format("-------------------------------- is_proper ---------------------------------~n"),                                
    lists:map (fun (Numero) ->                                                                                                                                                                                                                                                                                            
                        io:format("~p -> ~p ~n", [Numero, is_perfect(Numero)]) end,                                                                                                                                                                                                                                       
                        InputTestNumbers).                                                                                      
                                                                                                                                                                                                                                                                                                                        
                                                                                                                                
%----------------------------------------- is_palindrome ----------------------------------------------------                   
                                                                                                                                
is_palindrome(S) ->                                                                                                             
    L = string:casefold                                                                                                         
        (lists:filter(fun(X) ->                                                                                                 
                        not lists:member(X,[$.,$,,$;,$?,32]) end,                                                               
                        S)),                                                                                                    
    string:equal(lists:reverse(L),L).                                                                                           
                                                                                                                                
%----------------------------------------- is_an_anagram ----------------------------------------------------           
                                                                                                                        
is_an_anagram(S, [H|T]) ->                                                                                              
    S1 = lists:sort(string:casefold (lists:filter(fun(X) ->                                                             
                        not lists:member(X,[$.,$,,$;,$?,32]) end, S))),                                                 
    S2 = lists:sort(string:casefold (lists:filter(fun(X) ->                                                             
                        not lists:member(X,[$.,$,,$;,$?,32]) end, H))),                                                 
    if(S1 == S2) -> true;                                                                                               
    true -> is_an_anagram(S, T)                                                                                         
    end;                                                                                                                
is_an_anagram(_, []) ->                                                                                                 
    false.                                                                                              
                                                                                                        
%testing                                                                                                
                                                                                                        
                                                                                                             
%----------------------------------------- is_prime ----------------------------------------------------
                                                                                                        
is_prime(N) ->                                                                                               
    is_prime(N, 2).                                                                                
                                                                                                              
is_prime(N, Div) -> %testa se un numero e' divisibile per tutti i numeri compresi fra Div (che e' 2) e N                                                                                                           
    if (Div >= N ) -> true;  %se ho finito i numeri true                                                                                               
    true ->                                                                                                                     
        if ((N rem Div) == 0) -> false; %se e' divisibile per un numero allora false                                                                                         
        true ->                                                                                                   
            is_prime(N, Div +1)                                                                                              
        end                                                                                                                     
    end.                                                                                                                        
                                                                                                                                
%----------------------------------------- factors ----------------------------------------------------           
                                                                                                                  
factors(N) ->                                                                                                     
    lists:filter (fun(X) -> is_prime(X) end, factors (N, 1)). % fra tutti i divisori filtro solo quelli primi                                                                                              
                                                                                                                  
factors(N, Div) ->  % trova tutti i divisori di N                                                                                                             
    if (Div > N) -> [];                                                                                                          
    true ->                                                                                                                                                   
        if((N rem Div) == 0) ->                                                                                       
            [Div | factors(N, Div+1)];                                                                                
        true ->                                                                                                                                   
            factors(N, Div + 1)                                                                                         
        end                                                                                                                 
    end.                                                                                                                        
                                                                                                                      
                                                                                                                      
%----------------------------------------- is_proper ----------------------------------------------------             
                                                                                                                      
componi(From, From) -> []; %funzione per la composizione di una lista [da a]                                                                                                
componi(From, To) ->                                                                                                            
        [From|componi(From+1, To)].                                                                                   
                                                                                                                      
is_perfect(X) ->                                                                                                      
    Lista = componi(1,X), % lista di interi che va da 1 a X                                                                              
    Divisori = lists:filter(fun(N) -> (X rem N) == 0 end, Lista),  % filtro solo i divisori di X                                                              
    lists:sum(Divisori) == X.  % controllo che la somma dei divisori sia uguale a X                                                                 