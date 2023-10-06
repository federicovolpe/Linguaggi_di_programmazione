import scala.util.parsing.combinator._                                                     
                                                                                           
sealed trait Comando                                                                       
case class Singolo(x:Int, y: Int) extends Comando                                          
case class Combinazione(n: Int, comandi: List[Comando]) extends Comando                    
                                                                                           
object brucoInterpreter{                                                                   
                                                                                           
    def execute(comandi: List[Comando], x: Int = 0, y: Int = 0): (Int, Int) = comandi match{
        case h :: t =>                                                                     
            val c = h.asInstanceOf[Singolo]                                                
            execute(t, x + c.x, y + c.y)                                                   
        case _ =>                                                                          
            (x,y)                                                                          
    }                                                                                      
}                                                                                          
                                                                                           
object brucoCompiler{                                                                      
    def compile(comandi: List[Comando]) : List[Comando] = comandi match{                   
        case h :: t if h.isInstanceOf[Combinazione] =>              
            val c = h.asInstanceOf[Combinazione]                    
            compile(dispiega(c.n, c.comandi)) ++ compile(t)                  
        case h :: t =>                                              
            h :: compile(t)                                         
        case _ => Nil                                               
    }                                                               
                                                                    
    def dispiega (n: Int, c: List[Comando]): List[Comando] = n match{
        case 2 =>                                          
            c ++ c                                         
        case n =>                                          
            c ++ c                                                      
            dispiega(n-1, c)                                               
    }                                                               
                                                                    
}                                                                   
                                                                    
object brucoParser extends JavaTokenParsers {                          
    def parse:(Parser[List[Comando]]) = rep1(comando | combinazione)                       
                                                                       
    def comando:(Parser[Singolo]) = ("↑"|"↓"|"→"|"←") ^^ {                  
        case "↑" => Singolo(0,1)                                       
        case "↓" => Singolo(0,-1)                                      
        case "→" => Singolo(1,0)                                       
        case "←" => Singolo(-1,0)                                      
    }                                                                  
                                                                       
    def combinazione: Parser[Combinazione] = wholeNumber ~ "(" ~ rep(parse) ~ ")" ^^ {
        case n ~ _ ~ comandi ~ _ => Combinazione(n.toInt, comandi.flatten)
    }                                                           
}                                                                      
                                                                       
object main{                                                                               
    def main(args: Array[String]) : Unit = {                                               
        //lettura del file                                                                 
        args.foreach{ filename =>                                                          
                                                                                           
        var testo = io.Source.fromFile(filename).mkString                                  
                                                                                           
        //parsing                                                                          
        brucoParser.parseAll(brucoParser.parse, testo) match{                              
            case brucoParser.Success(res, _) =>                                  
                println("risultato: "+ res.foreach{println})                     
                val interpretato = brucoCompiler.compile(res)                    
                println("dopo l'interpretazione :" + interpretato.foreach{println})       
                val exec = brucoInterpreter.execute(interpretato)
                println("dopo l'esecuzione: " + exec)
                                                                                           
            case brucoParser.NoSuccess(err, _) => println("errore: "+ err)                               
        }                                                                                  
        }                                                                                  
    }                                                                                      
}                                                                                          