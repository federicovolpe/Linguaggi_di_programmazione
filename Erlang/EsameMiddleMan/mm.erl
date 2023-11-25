-module(mm).                                                      
-export([create/2, loop/1]).                                      
                                                                  
create(N, Server) ->                                              
    process_flag(trap_exit, true),                                            
    link(Server),                                                            
    List = [self()],                                                                                          
    io:format("Creazione avvenuta del middle man [~p]~n", List),                                              
    loop(N).                                                                                                     
                                                                                                                 
loop(N) ->                                                                                                     
    receive                                                                                                      
    %riceve un messagigo e lo invia al sserver con il porprio pid                                                
        {'EXIT', Pid, Reason} ->                                                                                 
            io:format("terminazione del middle man ~p dato che [~p di ~p]~n",[self(), Reason, Pid]),                                                                                       
            exit(normal);                                                    
                                                                                             
        {S, Destinatario} ->                                                                                     
            io:format("il middle man ~p ha ricevuto la stringa ~p~n",[N, S]),                                 
            Destinatario ! {N ,S}                                                                                                
                                                                                                                 
    end,                                                                                                         
    loop(N).                                                                                                          