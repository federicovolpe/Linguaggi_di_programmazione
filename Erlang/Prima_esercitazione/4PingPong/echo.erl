- module(echo).                                                                                         
- export ([start/0, print/1, stop/0, loopPadre/0, loopFiglio/0, setupFiglio/0]).                                     
                                                                                                          
start() ->                                                                                                
    register(process1, spawn(?MODULE, loopPadre, [])),                                                    
    register(process2, spawn(?MODULE, setupFiglio, [])),                                                         
    io:format("inizializzati i processi ~n").                             
                                                                          
stop() ->                                                                                                 
    process1 ! stop.                                                                                     
                                                                                                                       
print(Message) ->                                                                                         
    process1 ! {msg, Message}.                                                            
                                                                                                          
setupFiglio() -> % linko il processo figlio al padre                                                          
    io:format("processo figlio: ~p~n",[self()]),                                                                                                                                                                          
    process_flag(trap_exit, true),                      % attivo la trapExit per le terminazioni anomale del padre                                                                                                                                                       
    Pid = whereis(process1),                            % ricavo il pid del processo padre                                                                                                                                               
    link(Pid),                                          % link del processo al processo padre                                                                                                                                               
    loopFiglio().                                                                                            
                                                                                                          
                                                                                                     
loopPadre() ->                                                                                                    
    receive                                                                                         
        stop ->                                                                                                                                                                                 
            io:format("~p sta terminando ~n", [self()]),                                                                                                                                
            exit(killChild);                                                                      
                                                                                                                                                                                                                                                                         
        {msg, Message} ->                                                                                                                                                                              
            io:format("~p ha ricevuto il messaggio ~p ~n", [self(), Message])                                                                                                                          
    end,                                                                                                                                                                                               
    loopPadre().                                                                                        
                                                                                                    
                                                                                                                                                                                           
loopFiglio() -> % come da esercizio, l'unico scopo e' quello di terminare una volta che il padre termina                                                                                                  
    receive                                                                                                                                                                                
        {'EXIT', _Pid, Reason} ->                                                                                                                                                          
            io:format("il processo padre ~p sta terminando con la reason: ~p~n", [self(), Reason]),
            exit(normal)       % terminazione del figlio                                                                                         
                                                                                                                                                                                           
    end,                                                                                                                                                                                               
    loopFiglio().                                                                                                                                                                          
                                                                                                                                                                                                       
                                                                                                                                                                                           