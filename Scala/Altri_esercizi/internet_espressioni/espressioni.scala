/*                                                                                      
: Int    costruire un semplice parser che presa una stringa rappresentante una espressione
    la suddivida e ne calcoli l'output                                                  
*/                                                                                      
import scala.util.parsing.combinator.JavaTokenParsers    
import scala.io.StdIn                  
                                                                                        
case class Espressione(valore: Int, op: Option[Operazione])                             
                                                                                        
sealed trait Operazione                                                                                  
    case class Addizione(a: Espressione, b: Espressione) extends Operazione                                              
    case class Sottrazione(a: Espressione, b: Espressione) extends Operazione                                            
                                                                                                                    
                                                                                                                    
//parser per la stringa                                                                                              
object ExpParser extends JavaTokenParsers {    
                                                                                
    def esp : Parser[Espressione] = wholeNumber                                                                               
                                                                                                                     
    def sottrazione : Parser[Sottrazione] = esp ~ "-" ~ esp ^^ {case sx ~ _ ~ dx => Sottrazione(sx, dx)}            
                                                                                                                     
    def addizione : Parser[Addizione] = esp ~ "+" ~ esp ^^ {case sx ~ _ ~ dx => Addizione(sx, dx)}                  
                                                                                                                    
}                                                                                                                    
                                                                                                                     
object main extends App{                                                                                             
    //lettura di una stringa                                                                                         
    val input = StdIn.readLine                                                                                       
    while(input.nonEmpty){                                                                                           
        println("inserisci la funzione da parsare")                                                                  
        ExpParser.parseAll(ExpParser.esp, input)                                                                    
//        input = StdIn.readLine()                                                                                        
    }                                                                 
}                                                            