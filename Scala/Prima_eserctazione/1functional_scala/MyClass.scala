object MyClass {                                                                                
    def main(args: Array[String]) : Unit = {                                                    
        val prova : String = "ttoga"                                                            
        val lista = List[String] ("ciao", "gatto","mammam", "taco", "rincoglionito", "merda")
           
        // TEST PALINDROME                                                                      
        lista.map(x => f"è palindromo $x ? -> ${is_palindrome(x)}\n").foreach{print}            
                                                                                                
        // TEST ANAGRAMMI                                                                       
        lista.map(x => f"anagrammi $x e $prova ? -> ${is_an_anagram(x,prova)}\n").foreach{print}
                                                                                             
        // TEST FATTORI                                                                         
        var risultato : Array[Int] = Array(1)                                                   
        factors(14, 14, 2, risultato).foreach{println}                                          
        println("fattori2:")                                                                     
        factors2(496).foreach{println}                                                           
        for(i <- 0 to 1200000){                                                                  
            is_proper(i)                                                                         
        }                                                                                        
    }                                                                                            
                                                                                                 
    /* ritorna true solo se e' palindorma*/                                                                                          
    val is_palindrome = (s : String) => {                                                        
        val s1 = s.filterNot( /* stringa filtrata dei caratteri speciali e messa tutta uppercase */                                 
                        x => List('.',',',' ','?',';').contains(x))                              
                  .toUpperCase()                                                                 
                                                                                                 
        s1.equals(s1.reverse)                                                                    
    }                                                                                            
                                                                                               
    /* ritorna true solo se le due stringhe sono anagrammi*/                                                                           
    val is_an_anagram = (s: String, s2: String) => { s1.toList.sorted.equals(s2.toList.sorted) }
                                                  
    /**                                       
    * Calcola i fattori di un numero dato e li memorizza in un array.
    *                                         
    * Questa funzione calcola i fattori di un numero specificato (prog) utilizzando
    * il metodo della divisione per divisioni successive. Gli eventuali fattori
    * trovati vengono memorizzati in un array.
    *                                         
    * @param orig Il numero originale di cui si desidera trovare i fattori.
    * @param prog Il valore corrente da esaminare durante la divisione.
    * @param divisore Il divisore attuale da provare.
    * @param fattori L'array in cui memorizzare i fattori trovati.
    * @return Un array contenente i fattori del numero originale.
    */                                                                                            
    def factors (orig: Int, prog: Int, divisore: Int, fattori: Array[Int]): Array[Int] = {        
        if (divisore > orig || prog == 0){                                                       
            fattori                                                                              
        } else {                                                                               
            if (prog % divisore == 0){                                                         
                factors(orig, prog/divisore, divisore, fattori:+divisore)                      
            } else {                                                             
                factors(orig, prog , divisore+1, fattori)                        
            }                                                                    
        }                   
    }                       
                                                                       
    /**                                                                
    * Calcola i fattori di un numero dato e li restituisce come lista. 
    *                                                                  
    * Questa funzione calcola i fattori di un numero specificato (num) utilizzando
    * il metodo della divisione per divisioni successive. Restituisce i fattori
    * trovati come una lista.                                          
    *                                                                  
    * @param num Il numero di cui si desidera trovare i fattori.       
    * @param divisore Il divisore attuale da provare (predefinito a 2).
    * @param fattori Una lista che contiene i fattori trovati (predefinita come lista vuota).
    * @return Una lista contenente i fattori del numero specificato.   
    */                                                                 
    def factors2 (num: Int, divisore: Int = 2, fattori :List[Int] = Nil) : List[Int] = {
        LazyList                                                       
        .iterate(divisore)(i => i+1) //crea una lista che parte dal divisore allinfinito
        .takeWhile(n => n <= num) //seleziona i numeri che sono inferiori al target
        .find(n => num % n == 0 ) //seleziona i numeri che sono divisori del numero
        .map(n => factors2(num/n, n, fattori :+ n)) //viene fatta la chiamata con il numero diviso per il divisore trovato 
        .getOrElse(fattori)                                                                        
    }                                                                                              
                                                                                                   
    /* determina se il numer n e' perfetto */                                                      
    def is_proper (n:Int): Unit = {                                                                
        if (divisors(n).sum == n) {                                                                
            println(s"$n è perfetto")                                                              
        }                                                                                          
    }                                                                                              
    /* usata per la precedente funzione, determina i divisori del numero n ritornandoli come lista */                                      
    def divisors(n: Int, divisore : Int = 1, res : List[Int] = Nil): List[Int] = {                 
        if(divisore >= n){                                                                         
            res                                                                                    
        }else if(n % divisore == 0){                                                               
            divisors(n, divisore+1, res:+divisore)                                                 
        }else{                                                                                     
            divisors(n, divisore+1, res)                                                           
        }                                                                                          
    }                                                                                              
}                                                                                                  
        