% esercizio terminato
-module(server).                                    
-export([start/0, loop/1]).                         
                                                    
start() -> %funzione che crea il processo server   
    register(server, spawn(?MODULE, loop,[[]])).
                                                    
                                                    
loop(Servizi) ->                                    
    receive                                         
        d0 ->                                  
            loop(aggiungi(dummy0, Servizi));                               
        d1 ->                                  
            loop(aggiungi(dummy1, Servizi));             
        d2 ->                                  
            loop(aggiungi(dummy2, Servizi));                         
        tot ->                                                     
            io:format("servizi chiamati : ~p~n",[Servizi]),              
            loop(Servizi); %ritorna il numero di chiamate che sono state fatte
        kill -> unregister(server),exit(normal);                   
        Other ->                                                   
            io:format("~pnon è una funzione supportata~n",[Other])
                                                                   
    end,                                                           
    loop(Servizi).                                 
                                                   
aggiungi(S, []) ->                                 
    [{S, 1}];                                        
aggiungi(S, [{S, N} | T]) ->               
    [{S, N+1} | T];                        
aggiungi(S, [H | T]) ->                    
    [H] ++ aggiungi(S, T).                 