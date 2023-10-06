import scala.util.parsing.combinator.JavaTokenParsers                                         
                                                                                               
//definizione delle operazioni del parser                                                                                                                           
sealed trait Operazione                                                                                                                                             
case class And(a: Boolean, b: Boolean) extends Operazione                                                                                                                
case class Or(a: Boolean, b: Boolean) extends Operazione                                             
                                                                                                
//definizione della classe parser                                                               
object BooleanParser extends JavaTokenParsers{                                                                                  
    //parser per un operando                                                                  
    def operatore : Parser[Boolean] = {                                                       
        "true" ^^ (_ => true) | "false" ^^ (_ => false)                                                               
    }                                                                                         
                                                                                              
    def operazione: Parser[Operazione] = operatore ~ ("&&" | "||") ~ operatore ^^ {                        
            case a ~ "&&" ~ b => And(a, b)                                                   
            case a ~ "||" ~ b => Or(a, b)                                                   
            case _ => throw new IllegalArgumentException("c'è stato un problema con il parser") 
        }                                                                                                                                                                                      
}                                                                                               
                                                                                                
//classe per l'esecutore, questa prevede funzioni per operare sul risultato che verrà restituito alla fine                                                          
object BooleanExecuter{                                                                                                                                                    
    //funzioni con cui posso operare                                                                                                                                
    def and (a: Boolean, b : Boolean): Boolean = { a && b }                                                                                                         
                                                                                                                                                                    
    def or (a: Boolean, b : Boolean ): Boolean = { a || b }                                                                                                         
                                                                                                                                                                    
    //funzione per eseguire tutto                                                                                                                                   
    def execute (operazione : Operazione) : Boolean = operazione match{                                                                                                           
        case And(a,b) => and(a, b)                                    
        case Or(a,b) => or(a, b)                                      
    }                                                                                                            
}                                                                                                                
                                                                                                                 
object boolean extends App{                                                                                      
    val scanner = io.StdIn.readLine() //scala.io.Source.fromFile("operazioni.txt").mkString                                                  
                                                                                                                 
    BooleanParser.parseAll(BooleanParser.operazione, scanner) match{                                                           
        case BooleanParser.Success(risultato, _) =>                                                     
            println("l'esecuzione è avvenuta con successo: ")                                           
            BooleanExecuter.execute(risultato.asInstanceOf[Operazione])                                 
                                                                                                        
        case e => println("il parser ha fallito")                                                                                          
    }                                                                                                     
}                                                                                                         