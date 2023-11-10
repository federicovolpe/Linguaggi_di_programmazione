% esercizio non ancora terminato
-module(es2).                                                       
-export([main/0]).                                                  
                                                                    
main() ->                                                            
      parse("((2+3)-4)") .                                          
                                                                    
% parser da ((2+3)-4) a {minus, {plus, {num, 2}, {num,3}}, {num, 4}}
                                                         
parse([N1, "+", N2 | T]) ->                            
    {plus, parse(N1), parse(N2)} ;                     
                                                       
parse([N1, "-", N2 | T]) ->                            
    {minus, {num, list_to_integer(N1)}, parse([N2 | T])};
                                                       
parse(["(" | T]) ->                                    
    {Expression, Rest} = parse(T),                     
    {Expression, tl(Rest)};                            
                                                       
parse([")" | T]) ->                                    
    {{num, 0}, T};                                     
                                                       
parse([N | T]) when is_integer(N) ->                   
    {{num, N}, T}.                                                               