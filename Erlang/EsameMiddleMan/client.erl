% riceve le risposte del server                                        
% invia la richiesta al server                                         
-module(client).                                                       
-export([create/0, loop/1, send_request/1]).                           
                                                                       
                                                                       
create() ->                         
    Client = spawn(client, loop, [[]]),
    ServerPid = spawn(server, start, [Client]),
                                    
    case register(srv, ServerPid) of
        true ->                     
            io:format("Server process registered as srv~n");
        false ->                    
            io:format("Failed to register server process~n")
    end,                            
                                    
    case register(c, Client) of     
        true ->
            io:format("Client process registered as c~n");
        false ->                                                                    
            io:format("Failed to register client process~n")                        
    end,                                                                            
                                                                                    
    io:format("Creation of server [~p] and client [~p] completed~n", [ServerPid, Client]),
    ok.                                                                             
                                                                                                                                
                                                                                                                                
send_request(Request) ->                                                                                                        
    srv ! {self(), Request}.                                                                                                    
                                                                                                                                
loop(Recieved) ->                                                                                                               
    io:format("processo client [~p] pronto per ricevere ~n",[self()]),                                                          
    receive                                                                                                                     
        {snd, Request} ->                                                                                                       
            send_request(Request),                                                                       
            loop(Recieved);                                                                         
                                                                                                                                
        {Mittente , Risposta} ->                                                                                                   
            io:format("il client ha ricevuto la risposta [~p] dal mittente ~p~n", [Risposta, Mittente]),                   
                                                                                                                                
            if length(Recieved) > 0 ->                                                                                                    
                    io:format("risposta finale ~p~n",[componi([{Mittente, Risposta}| Recieved])]),
                    loop([]);                                                                       
                true ->                                                                                                         
                    loop([{Mittente, Risposta}])                                                                      
            end;                                                                                                                
                                                                                                                                
        stop ->                                                                                              
            unregister(master),                                                                              
            io:format("terminazione del client [~p]~n",[self()]),                                            
            exit(kill)   % il processo figlio, master viene ucciso                                                         
    end.                                                                                          
                                                                                               
componi([{_ , Risposta}]) ->                                         
    Risposta;                                                        
componi([{1 , Risposta} | T]) ->                                     
    Risposta ++ componi(T);                                          
componi([{2 , Risposta} | T]) ->                
    componi(T) ++ Risposta.                     