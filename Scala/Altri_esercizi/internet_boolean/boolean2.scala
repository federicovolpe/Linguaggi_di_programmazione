import scala.util.parsing.combinator._                 
                                                       
// definizione delle classi                                                                            
sealed trait Espressione                                                                               
case class And(a: Boolean, b: Boolean) extends Espressione                                             
case class Or(a: Boolean, b: Boolean) extends Espressione                                              
case class Esp(sx: Espressione, dx: Espressione) extends Espressione                                   
                                                                                                       
//parser per una espressione                                                                           
object BooleanParser extends JavaTokenParsers{                                                                                  
    //parser per un valore booleano                                                                    
    def bool : Parser[Boolean] = "v" ^^ (_ => true) | "f" ^^ (_ => false)            
                                                                                            
    /*parser per una operazione binaria                                                     
    def esp : Parser[Espressione] = bool ~ ("&&"|"||") ~ bool ^^ {                            
        case s ~ "&&" ~ d => And(s, d)                                                        
        case s ~ "||" ~ d => Or(s, d)                                                         
        case _ => throw new IllegalArgumentException("problema del parser della operazione")  
    } */                                                                                                                                
                                                                  
    def esp: Parser[Espressione] = bool ~ rep("&&" ~> bool | "||" ~> bool) ^^ {
        case s ~ list => list.foldLeft(s)((acc, b) => Esp(acc, b))
    }                                                                                                                      
    def parentesi : Parser[Espressione] = "(" ~> esp <~ ")"                                                                                 
                                                                                                                                            
    def parse: Parser[Espressione] = parentesi | esp | bool                                                                                               
}                                                                                                                                           
                                                                                                                                            
object BooleanEvalutator{                                                                                                                   
   /* def execute(esp: List[Espressione]): Boolean = {                                                                                        
        esp.foreach{espressione => espressione match{                                                                                       
                case And(a, b) => a && b                                                                                                    
                case Or(a, b) => a || b                   
                case _ => throw new IllegalArgumentException("c'è stato un problema nell'evaluator")                                        
            }                                                                                                                               
        }                                                                                                                                   
    }  */                                                                                                                                     
}                                                                                                                                           
                                                                                                                                            
object boolean2 extends App{                                                                                                                
    //lettura dallo standard input                                                                                                          
    val testo = io.StdIn.readLine()                                                                                                         
    BooleanParser.parseAll(BooleanParser.esp , testo) match{                                                                                
        case BooleanParser.Success (risultato, _) =>                                                                                   
            println("parser eseguito con successo risultato :" + risultato)                                                       
                                                                                                                                  
                                                                                                                                  
        case e => println("errore nel parser")                                                                                    
    }                                                                                                                             
}                                                                                                                                 