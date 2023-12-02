- module (mp).                                                                                 
- export ([long_reverse_string/1, loop_slave/2]).                                                   
                                                                         
% funzione principale, unica funzione da chiamare
% il processo master e' il processo corrente                                                                                             
long_reverse_string(S) ->                                                                      
    Processes = create_processes(10, self()),   
    io:format("lunghezza della stringa: ~p diviso 10 : ~p~n",[length(S), length(S) div 10]),                                                             
    send(S, Processes, length(S) div 10), % invia un decimo del problema ad ogni processo slave                                                   
    wait_answers(1).                     % loop per aspettare le risposte                                                                       
                                                                                               
create_processes(1, Master) ->                                                                 
    spawn(?MODULE, loop_slave, [1, Master]);                                                   
create_processes(N, Master) ->                                                                 
    [spawn(?MODULE, loop_slave, [N, Master]) | create_processes(N-1, Master)].                 
                                                                                               
% invia a tutti i processi in lista una parte del problema ( un decimo)                                                                             
send(S, [H | T], Lenght) ->                                                                                                          
    H ! string:slice(S, 0, Lenght),                                                   
    send(string:slice(S,Lenght,length(S)), T, Lenght);                     
send(S, H, _) ->                                                                    
    H ! S.                                                                 
                                                                          
% del processo master si occupa di ricevere tutte le 10 soluzioni dagli slave                                                                                                         
wait_answers(N) ->
    receive
        {Answer, N} when N == 1->
            io:format("master riceve ~p da ~p~n", [Answer,N]),
            Answer ++ wait_answers(N + 1);
        {Answer, N} when N == 10 ->
            io:format("master riceve ~p da ~p~n", [Answer,N]),
            Answer;
        {Answer, N} ->
            io:format("master riceve ~p da ~p~n", [Answer,N]),
            Answer ++ wait_answers(N + 1)
    end.
                                                                                                                                
                                                                                            
% tiene memorizzato il suo numero progressivo di processo e il master a cui inviare la soluzione elaborata                                                                                                                                          
loop_slave(N, Master) ->                                                                                                                   
    receive       
                                                                                                                              
        Msg ->  io:format("il processo ~p riceve ~p e risponde ~p~n",[self(), Msg, {string:reverse(Msg), N}]),   
               Master ! {string:reverse(Msg), N}                                                                                           
    end.                                                                                                                                   
