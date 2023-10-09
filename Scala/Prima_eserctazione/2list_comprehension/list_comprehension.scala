class Funzionilc {                                                                                                                                   
    val squared = (l :List[Any]) => {                                                                                                                
        //siccome collect è una funzione parziale ha bisogno di {}                                                                                   
        //collect usa il pattern mathcing se l'elemento n rispetta una certa caratteristicaì                                                         
        l.collect{case n: Int => n*n }                                                                                                               
    }                                                                                                                                                
                                                                                                                                                     
    //metodo che prese due liste ritorna la lista degli elementi compresi in entrambi                                                                 
    val intersect = (l1: List[Any], l2: List[Any]) => {                                                                                                  
        l1.filter(x => l2.contains(x))                                                                                                                 
    }                                                                        
                                                                             
    val symmetric_difference =  (l1: List[Any], l2: List[Any]) => {                         
        l1.diff(l2).concat(l2.diff(l1))                                                           
    }                                                                        
}                                                                                                                                                       
                                                                                                                                                        
object main extends App{                                                                                                                                
                                                                                                                                                        
    val l1: List[Any] = List(1,2,34,"hello","c",'f')                                                                                                                                    
    val l2: List[Any] = List(1,25,2,"beac","hello",'f','v')                                                                                                          
    val f :Funzionilc = new Funzionilc                                                                                                                  
                                                                                                                                                        
    println("lista filtrata: "+ f.squared(l1))                                                                                                          
    println("intersezione della lista " + l1 +"\ncon la lista " + l2 +"\n = "+ f.intersect(l1,l2))                                                         
    println("differenza delle due liste: \n"+ f.symmetric_difference(l1,l2))                                                            
}                                                                                                                                                       