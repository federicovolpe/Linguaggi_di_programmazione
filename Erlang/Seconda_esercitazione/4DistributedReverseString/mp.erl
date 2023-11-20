- module (mp).                                                                                 
- export ([long_reverse_string/1, loop_slave/2]).                                                   
                                                                         
% funzione principale, unica funzione da chiamare
% il processo master e' il processo corrente                                                                                             
long_reverse_string(S) ->                                                                      
    Processes = create_processes(10, self()),                                                                
    send(S, Processes, length(S) div 10), % invia un decimo del problema ad ogni processo slave                                                   
    wait_answers([]).                     % loop per aspettare le risposte                                                                       
                                                                                               
create_processes(1, Master) ->                                                                 
    spawn(?MODULE, loop_slave, [1, Master]);                                                   
create_processes(N, Master) ->                                                                 
    [spawn(?MODULE, loop_slave, [N, Master]) | create_processes(N-1, Master)].                 
                                                                                               
% invia a tutti i processi in lista una parte del problema ( un decimo)                                                                             
send(S, [H | T], Lenght) ->                                                                    
    io:format("invio a ~p il messaggio ~p~n",[H, string:slice(S, 0, Lenght)]),                                                       
    H ! string:slice(S, 0, Lenght),                                                   
    send(string:slice(S,Lenght,length(S)), T, Lenght);                     
send(S, H, _) ->                                                                     
    H ! S.                                                                 
                                                                          
% del processo master si occupa di ricevere tutte le 10 soluzioni dagli slave                                                                                                         
wait_answers(Solutions) ->                                                                                 
    receive                                                                                                
        {Answer, Pnum} ->                                                                                  
            io:format("ricevuta la risposta ~p da ~p~n", [Answer, Pnum]),                                  
            Length = length([{Pnum, Answer} | Solutions]),                                               
            if(Length == 10) ->   % se ho ricevuto tutte e 10 le soluzioni                                                                                                         
                Soluzione = ricomponi([{Pnum, Answer} | Solutions]),                                                                       
                io:format("la soluzione ricavata: ~p~n", [Soluzione]);                                                                     
                                                                                                                                           
            true -> wait_answers([{Pnum, Answer} | Solutions])                                                                             
            end                                                                                                                            
    end.                                                                                                                                   
                                                                                                          
% ricompone le soluzioni ricevute dal processo                                                                                                                                           
ricomponi(L) ->                                                                                                                            
    Sorted = lists:sort(fun ({A, _}, {B, _}) -> A < B end, L), % riordina la lista {num_processo , soluzione} in base ai numeri di processo    
    lists:foldl(fun ({_, S}, Acc) -> Acc ++ S end,"", Sorted). % estrapola solo le soluzioni dalla lista precedente e le unisce                                                                           
                                                                                                          
% tiene memorizzato il suo numero progressivo di processo e il master a cui inviare la soluzione elaborata                                                                                                                                          
loop_slave(N, Master) ->                                                                                                                   
    receive                                                                                                                                
        Msg -> Master ! {string:reverse(Msg), N}                                                                                           
    end.                                                                                                                                   