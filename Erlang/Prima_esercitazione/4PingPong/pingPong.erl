% NON L'ESERCIZIO PREVISTO DALLA TRACCIA
-module (pingPong).                                                 
-export ([start/1,ping/0, pong/0]).                                 
                                        
                                                                
start(Ngiri) -> % creazione e registrazione dei processi ping e pong
    register(ping, spawn(?MODULE, ping, [])),                       
    register(pong, spawn(?MODULE, pong, [])),                       
    ping ! Ngiri.  % inizio del ciclo tramite un messaggio                                                 
                                                                    
ping() ->                                                           
    receive                                                         
        0 -> % terminate the execution                              
            io:format("terminating the execution"),                 
            pong ! stop,                                            
            exit(normal);                                           
        N -> % continue the execution                     
            io:format("ping ~p ~n", [N]),                 
            pong ! N                                      
    end,                             
ping().                              
                                     
pong() ->                            
    receive                          
        stop ->                      
            exit(normal);            
                                     
        N -> %sends back the message
            io:format("pong ~p ~n", [N]),
            ping ! N - 1
    end,                
pong().                 
                        