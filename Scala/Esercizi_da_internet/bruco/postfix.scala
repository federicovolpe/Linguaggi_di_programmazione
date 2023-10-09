import scala.util.parsing.combinator._                                                
                                                                            
sealed trait Token                                                                                                                                             
case class num (valore : Int) extends Token                                                      
case class symbol (simbolo : String) extends Token                                                  
                                                                                      
object arithmeticParser extends JavaTokenParsers {                                                                 
                                                                                      
    def espressione: Parser[List[Token]] = rep1(numero | simbolo)                                                                       
                                                                                     
    def numero : Parser[num] = wholeNumber ^^ {case n => num(n.toInt)}               
                                                                                     
    def simbolo : Parser[symbol] = """[\-+/*()]""".r ^^ {case s => symbol(s)}        
}                                                                                    
                                                                                     
object arithmeticInterpreter {                                                             
                                                                                      
    def divisione (l: (List[num], List[symbol])): Unit = {  
        val numbers : List[num]                             
        val symbols : List [symbol]                         
        // prende le due liste e performa le operazioni                                         
        l.foreach{ x =>                                                           
            case x : num => numbers :+ x                                            
            case x : symbol =>                              
                    match x                                 
                    case x.simbolo == ")" => symbols.togli                                   
                //pop all the elements untill the ( which is removed                                                               
                simboli :+ l.head                                                
            }                                                                    
            divisione(l.tail)                                                    
        //} else{                                                    
          //  println("numeri: " + numeri + "\noperazioni: " + simboli)
        }                                                                                
    }                                                                                                 
}                                                                                                     
                                                                                                      
object main{                                                                                          
    def main(args: Array[String]){                                                                    
        args.foreach{fileName =>                                                                      
            val testo = io.Source.fromFile(fileName).mkString                                         
                                                                                                      
            arithmeticParser.parseAll(arithmeticParser.espressione, testo) match {                    
                case arithmeticParser.Success(res, _) => println("success : " + res)  
                                                                                                      
                case arithmeticParser.NoSuccess(e, _) => println("err: " + e)                         
            }                                                                                         
        }                                                                                             
    }                                                                                                 
}                                                                                                     