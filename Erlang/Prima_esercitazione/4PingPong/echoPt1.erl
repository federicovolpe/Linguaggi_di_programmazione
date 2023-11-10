- module(echoPt1).                                                                                         
- export ([start/0, print/1, stop/0, loop/0]).                                                                  
                                                                                                        
% l'internal message protocol per fare in modo che il comando print(stop) non faccia fermare il processo
% prevedera' l'invio di una tupla per i messaggi composta da: {msg, Messaggio}.     
                                                                                                        
start() ->                                                                                              
    register(process1, spawn(?MODULE, loop, [])), % inizializzo il primo processo    
    io:format("creato il processo ~p ~n", [process1]),             
    ok.                                                                             
                                                                                                        
stop() ->                                                                                               
    process1 ! stop.                                                                                     
                                                                                
                                                                                                    
print(Message) ->                                                                                       
    process1 ! {msg, Message}.                                                                                  
                                                                                                        
loop() ->                                                                                                    
    receive                                                                         
        stop ->                                                                     
            io:format("process is terminating"),                                    
            exit(normal);                                                           
                                                                                    
        {msg, Message} ->                                                     
            io:format("ricevuto il messaggio ~p ~n", [Message])        
    end,                                                               
    loop().                                                            