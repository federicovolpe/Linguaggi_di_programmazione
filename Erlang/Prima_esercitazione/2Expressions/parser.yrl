Nonterminals expression factor.         
Terminals "+" "-" "(" ")" integer.      
                                        
Rootsymbol expression.                  
                                        
expression -> factor : '$1'.            
expression -> expression '+' factor : {plus, '$1', '$3'}.
expression -> expression '-' factor : {minus, '$1', '$3'}.
                                        
factor -> integer : {num, list_to_integer('$1')}.
factor -> '(' expression ')' : '$2'.    
                                        
ErlangTypes                             
    expression() -> term().             
    factor() -> term().                 
                                        