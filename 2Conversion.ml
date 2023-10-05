(*Write a function that given a pure number                                        
   returns a conversion table for it among any of the 8 scales*)                   
type tName = Celsius|Farenheit|Kelvin|Rankine|Delisle|Newton|Réaumur|Rømer ;;
type temperature = {name: tName; value: float};;                                
                                                                            
                                                                                   
let conversion x =                                                        
    match fst x with                                                      
        |Celsius    ->                                                        
        |Farenheit ->                                 
        |Kelvin    ->  
        |Rankine   ->
        |Delisle   -> 
        |Newton    -> 
        |Réaumur   -> 
        |Rømer     ->                                                               
                                                                                   