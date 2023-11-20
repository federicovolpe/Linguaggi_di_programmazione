- module(ms).                                                                                                
- export([start/1, to_slave/2, setup_master/1, loop_slave/1]).                                               
                                                                                                             
% funzione che spawna e registra il processo master                                                             
start(N) ->                                                                                                  
    register(master, spawn(?MODULE, setup_master, [N])).                                                     
                                                                                                             
% consente di far inviare un messaggio al master che a sua volta lo reindizzerà al processo slave interessato                                                   
to_slave(Message, N) ->                                                                                      
    master ! {Message, N}.                                                                                   
                                                                                                             
% inizializza i processi slave del master e fa andare il master in loop                                                                                                             
setup_master(N) ->                                                                                           
    loop_master(registra(N, self())).                                                                        
                                                                                                             
% funzione che inizializza tutti i processi figli e memorizza il loro Pid nella lista                         
registra(1, Master) ->                                                                                                                            
    Pid = spawn(?MODULE, loop_slave, [Master]),                                                                              
    io:format("regstrato il processo ~p con identificativo: ~p~n",[Pid, 1]),                                                                                       
    [{1,Pid}];                                                                                                
                                                                                                                            
registra(N, Master) ->                                                                                                                            
    Pid = spawn(?MODULE, loop_slave, [Master]),                                                                                          
    io:format("regstrato il processo ~p con identificativo: ~p e master ~p~n",[Pid, N, Master]),                                                                                       
    [{N,Pid} | registra(N-1, Master)].                                                                                                   
                                                                                                                                         
loop_master(Processes) ->                                                                                                                
    process_flag(trap_exit, true),                                                                                                                         
    receive                                                                                                                               
        {'EXIT', Pid, Reason} -> %il processo Pid è terminato, ne genero uno nuovo con lo stesso nome                                    
                            io:format("il master ha ricevuto un messaggio di uscita del processo ~p con reason ~p~n",[Pid, Reason]),     
                            loop_master(riavvia(Pid, Processes , self())); % loop con la nuova lista di processi dove N è stato riavviato                                                                                              
                                                                                                                                          
        {die, N} -> io:format("dico al processo ~p di terminare ~n",[N]),                                                                
                    send(N, Processes, die);                                                                                                              
                                                                                                                                          
        %se ricevo un messaggio normale lo rigiro al destinatario                                                                         
        {Messaggio, N} ->                                                               
            io:format("invio un messaggio al processo ~p ~n",[N]),                                               
            send(N, Processes, {Messaggio, self()});                                                                       
                                                                                                                                         
        Other -> io:format("messaggio ~p non valido ~n",[Other])                                                                          
    end,                                                                                                                                  
loop_master(Processes).                                                                                                                  
                                                                                        
% funzione usata dal master per trovare un processo nella lista di processi e inviargli il messaggio                                                                                     
send(N, [], _) ->                                                                                                                  
    io:format("processo ~p non trovato~n",[N]);                                                                                    
send(N, [{N, Pid} | _], Msg) ->                                                                                                    
    io:format("processo ~p trovato ! ~n", [N]),                                                                                    
    Pid ! Msg;                                                                                                                     
send(N, [_ | T], Msg) ->                                                                                                           
    send(N, T, Msg).                                                                                                               
                                                                                                                                   
% funzione per trovare il processo che ha segnalato la sua terminazione al master e riavviarlo, sostituendo il processo nella lista                                                                                        
riavvia(_, [], _) ->                                                                                                               
    ok;                                                                                                                            
riavvia(Pid, [{N, Pid} | T], Master) ->                                                                                            
    New = spawn(?MODULE, loop_slave, [Master]),                                                                                    
    io:format("riavviato il processo con identificativo: ~p e master ~p~n",[N, Master]),                                                                                       
    [{N,New} | T];                                                                                                                 
riavvia(Pid, [H | T], Master) ->                                                                                                   
    [H | riavvia(Pid, T, Master)].                                  
                                                                                                        
% loop di un processo slave, muore o stampa il messaggio                                                 
loop_slave(Master) ->                                                                 
    link(Master),                                                                                              
    receive                                                                                                          
        die -> % invio del messaggio di terminazione al master                                                       
                Master ! {termina, self()},                                                                          
                io:format("il processo ~p è stato terminato ~n",[self()]),                                           
                exit(terminato);                                                                                     
                                                                                                                     
        {Messaggio, Mittente} -> io:format("il processo ~p ha ricevuto il messaggio: ~p dal processo ~p ~n",[self(), Messaggio, Mittente])
    end,                                                                                                             
loop_slave(Master).                                                                                                  