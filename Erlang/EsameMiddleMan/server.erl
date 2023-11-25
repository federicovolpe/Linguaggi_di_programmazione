-module(server).                                                                     
-export([start/1]).                                                                  
                                                                                     
start(Client) ->                                                                     
    io:format("processo server veramente avvenuta~n"),                                                                   
    process_flag(trap_exit, true),                                                                         
    link(Client),                                                                                         
    register(mm1, spawn(mm, create, [1, self()])),                                                                 
    register(mm2, spawn(mm, create, [2, self()])),                                                                 
    loop().                                                                                               
                                                                                                          
loop() ->                                                                                                 
    receive                                                                                               
        {'EXIT', Pid , Reason} ->                                                                         
            io:format("il server termina perche' il client e' stato terminato [~p di ~p]~n",[Reason, Pid]),  
            exit(kill);                                                                                   
                                                                                                          
        {Mittente, Quesito} ->                                                                    
            io:format("il server ha ricevuto il quesito ~p dal client ~p~n", [Quesito, Mittente]),                                                                        
            is_palindrome(Quesito, Mittente)                                                               
    end,                                                                                                                   
    loop().                                                                                                                
                                                                                                                           
is_palindrome(S, Destinatario) ->                                                                                          
    %spezza l'input in due parti uguali                                                                                    
    Half = string:len(S) div 2,                                                                                            
    Prima = string:sub_string(S, 1, Half),                                                                                 
    Seconda = string:sub_string(S, Half +1, string:len(S)),                                                                   
    mm1 ! {string:reverse(Prima), Destinatario}, % invio la prima metà al primo middleman                                          
    mm2 ! {string:reverse(Seconda), Destinatario}. % invio la seconda metà al secnondo middleman ma in modo reverse                                                                                                                 
                                                                                                                                                     