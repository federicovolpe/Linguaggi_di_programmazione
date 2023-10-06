import scala.io.StdIn                                                                                                                                                                                                                      
import scala.util.parsing.combinator._                                                             
                                                                                                   
object ArithmeticParser extends JavaTokenParsers {                                                 
    def parse : Parser[(List[Int], Int)] =                                                           
    wholeNumber ~ rep1(numeri) ~ risultato ^^ {case a ~ b ~ r => (a.toInt :: b, r )}                     
                                                                                      
    def numeri : Parser[Int] = ("-" | "+") ~ wholeNumber ^^ {                         
        case "-" ~ n => println("riconosciuto il numero -" + n) ; - n.toInt                                                 
        case "+" ~ n => println("riconosciuto il numero " + n) ; n.toInt                                                   
        case _ => throw new java.lang.IllegalArgumentException("il parser dei numeri non ha funzionato")      
    }                                                                                 
                                                                                      
    def risultato : Parser[Int] = "=" ~> "-+".r ~> wholeNumber ^^ {case n =>                               
        println("riconosciuto il risultato " + n ) ; n.toInt}                                                                                                                                      
}                                                                                                                                                                                                  
                                                                
object ArithmeticEvaluator {                                    
    def eval (l: List[Int] , r: Int): (Int, Boolean) = {                  
        var risultato = 0                                       
        l.foreach{n =>                                          
                    risultato += n                              
                  }                                             
        (risultato, risultato == r)                             
    }                                                                                     
}                                                                                                                                                                      
                                                                                                                                                                       
object arithmetic_evaluator {                                                                                                                                                                                                              
    def main(args: Array[String]) : Unit ={                                                                                                                                                        
        args.foreach{file =>                                                                                                                                                                                             
            val scanner = io.Source.fromFile(file).mkString                                                                                                                                                                                     
            ArithmeticParser.parseAll(ArithmeticParser.parse, scanner) match{                                                                                                                                                                  
                case ArithmeticParser.Success((lista, risultato), _) =>                                                                                                                                     
                    println("parsato con successo" + lista + " risultato : " + risultato)           
                    val (r: Int, v: Boolean) = ArithmeticEvaluator.eval(lista, risultato) 
                    println("risultato : " + r + " ? " + v)                              
                                                                                         
                case e => println("ERRORE NEL PARSER !!!!") }                                                                                                                                                                                  
            }                                                                                                                                                                                      
    }                                                                                                                                                                                                               
}                                                                                                                                                                                                                                          