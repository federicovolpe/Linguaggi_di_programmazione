% esercizio terminato                                                                          
-module(server).                                                                               
-export([start/0, loop/1]).                                                                    
                                                                                               
start() -> % funzione che crea il processo server (per inviargli messagi basta fare server ! ...)  
    register(server, spawn(?MODULE, loop,[[]])).                                               
                                                                                                          
                                                                                                          
loop(Servizi) -> % servizi forniti dal server                                                                               
    receive                                                                                               
        servizio0 ->                                                                                      
            loop(aggiungi(servizio0, Servizi));                                                           
                                                                                                          
        servizio1 ->                                                                                      
            loop(aggiungi(servizio1, Servizi));                                                           
                                                                                                          
        servizio2 ->                                                                                      
            loop(aggiungi(servizio2, Servizi));                                                           
                                                                                                          
        tot -> %ritorna i servizi che sono stati richiesti con il numero di volte che sono stati richiesti                                                               
            io:format("servizi chiamati : ~p~n",[Servizi]),                                               
            loop(Servizi);                                                                                
                                                                                                          
        kill ->                                                                                           
            unregister(server),                                                                           
            exit(normal);                                                                                 
                                                                                                          
        Other ->                                                                                          
            io:format("~pnon è una funzione supportata~n",[Other])                                                                                              
    end.                                                                                                  
                                                                                                   
%funzione che aggiunge un servizio chiamato alla lista di servizi                                                                              
aggiungi(S, []) ->           % se non si trova nella lista aggiungi                                             
    [{S, 1}];                                                                                      
aggiungi(S, [{S, N} | T]) -> % se si trova nella lista incrementa                                  
    [{S, N+1} | T];                                                                                
aggiungi(S, [H | T]) ->      % se la testa non corrisponde al servizio cercato controlla nella coda 
    [H] ++ aggiungi(S, T).                                                                         