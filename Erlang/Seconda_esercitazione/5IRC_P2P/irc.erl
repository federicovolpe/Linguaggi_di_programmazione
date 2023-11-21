% esercizio solo inizializzato, non terminato
- module(chat_group).                                                
- export([start/2]).                                       
       
% funzione responsaile per inizializzare il Channerl con un nickname
start(C, Nick) ->                                                   
    process_flag(trap_exit, true),                               % trap exit dei processi figli                                    
    lib_chan_mm:controller(C, self()),                           % invia a C la tupla {setController, self}
    lib_chan_mm:send(C, ack),                                    % invia a C la tupla {send, ack}
    self() ! {chan, C, {relay, Nick, "I’m starting the group"}}, % invia a se setesso la tupla   
    group_controller([{C, Nick}]).                               % inizia a ricevere i messaggi conoscendo     
                                                                
delete(Pid, [{Pid, Nick}|T], L) -> {Nick, lists:reverse(T, L)};                                                                      
delete(Pid, [H|T], L)          -> delete(Pid, T, [H|L]);                     
delete(_, [], L)               -> {"????", L}.                                    
                                                                
group_controller([]) -> exit(allGone);                              
group_controller(L) ->                                              
    receive                                   
        % se riceve un comando di passaggio del messaggio sul processo C
        {chan, C, {relay, Nick, Str}} ->                                    
            lists:foreach(fun({Pid,_}) ->                           % a ciascun elemento di L
                lib_chan_mm:send(Pid, {msg,Nick,C,Str}) end, L),    % invia il messaggio Nick, processo controllore C, Messaggio
                group_controller(L);                                % loop con la                 
        
        % se riceve una richiesta di login al processo C
        {login, C, Nick} ->                                                 
            lib_chan_mm:controller(C, self()),                      % setta il controller del canale
            lib_chan_mm:send(C, ack),                               % send a C {send, ack}
            self() ! {chan, C, {relay, Nick, "I’m joining the group"}},% invia a se stesso il messaggio di join
            group_controller([{C,Nick}|L]);                         % aggiunge il nuovo processo alla lista dei controller

        % nel caso si riceva un messaggio di chiusura del nodo C
        {chan_closed, C} ->                                     
            {Nick, L1} = delete(C, L, []),                          % Nick = elemento tolto dalla lista, L1 nuova lista con la rimozione
            self() ! {chan, C, {relay, Nick, "I’m leaving the group"}},% invia il messaggio di uscita dal gruppo
            group_controller(L1);                                   % nuovo ciclo con la nuova lista

        Any ->                                  
            io:format("group controller received Msg=~p~n", [Any]), % il group controller stampa il messaggio ricevuto
            group_controller(L)                                     % ciclo
    end.                                    