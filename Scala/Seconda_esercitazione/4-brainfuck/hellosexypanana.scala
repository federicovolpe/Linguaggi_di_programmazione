//secondo tentativo con l'interprete brainfuck                                                 
import scala.util.parsing.combinator.JavaTokenParsers                       
import scala.util.matching.Regex                                                               
import scala.collection.mutable                                           
                                                                          
//5 realizzazione di un trait che fa da middleman il quale compito è solo definire le operazioni
sealed trait Comando                                                                         
    //creazione di una case class per comando                                                  
    case class IncrementPointer() extends Comando                              
    case class DecrementPointer() extends Comando                              
    case class IncrementData() extends Comando                                 
    case class DecrementData() extends Comando                            
case class PrintState() extends Comando                                   
    case class Print() extends Comando                                         
    case class Input() extends Comando                                         
    case class Loop(expression: List[Comando]) extends Comando                 
                                                                               
    // classe per contenere il programma sotto forma di lista di comandi                                                                      
    case class Program(expressions: List[Comando]) extends Comando              
                                                                               
//6 creazione del parser                                                       
// obiettivo: ci sono dei caratteri che simboleggiano dei comandi              
object brainfuckParser extends JavaTokenParsers{                                     
    //7 siccome brainfuck non separa in base agli spazi come un parser normale 
    //bisogna ridefinire il carattere separatore                               
    override protected  val whiteSpace: Regex = """[^<>\+\-\[\]\.#,]*""".r     
                                                                               
    //8 per ogni comando ricevuto assegno il comando corrispondente                                                             
    def comando: Parser[Comando] = ("<" |">"|"+"|"-"|"."|","|"#"| loop) ^^{    
        case ">" => IncrementPointer()                                         
        case "<" => DecrementPointer()                                          
        case "+" => IncrementData()                                           
        case "-" => DecrementData()                                            
        case "." => Print()                                                         
        case "," => Input()                                                    
        case Loop(espressione) => Loop(espressione)                           
        case _ => throw new IllegalArgumentException             
    }                                                                          
                                                                               
    //9 definizione di loop che aspetta di ricecevere il carattere             
    def loop: Parser[Loop] = "[" ~> rep(comando) <~ "]" ^^ {Loop}              
                                                                                                                          
    //10 definizione di program                                                                                           
    def program: Parser[Program] = rep(comando) ^^ {Program}                                                              
                                                                                                                            
}                                                                                                                           
                                                                                                                            
//11 creazione dell'environment                                                                                             
class Environment {                                                                                                         
    //12 come area di memoria viene utilizzata una hashmap                                                                     
    private val data = new mutable.HashMap[Int, Int].withDefault(_ => 0) //withDefault assegna alla mappa tutti i valori 0  
    private var pointer = 0 //puntatore che punta all'interno della mappa                                                                                                 
                                                                                                      
    //13 definisco le operazioni che devono essere fatte sulla mappa                                                                                                                            
    def incrementPointer(): Unit = pointer += 1                                                                             
    def decrementPointer(): Unit = pointer -= 1                                                                               
    def increment(): Unit = data(pointer) += 1  //ripesca dalla mappa il valore contenuto nel pointer                                                                         
    def decrement(): Unit = data(pointer) -= 1                                                                                
    def get(): Int = data(pointer)                                                                                            
    def get(n: Int): Int = data(n)                                                                                             
    def put(n: Int): Unit = data(pointer) = n                                                                                 
                                                                                                                                
    override def toString: String = f"^$pointer, " + data.toString()                                                          
}                                                                                     
                                                                                      
//14 definizione dell'interprete                                                      
object brainfuckInterpreter {                                                         
                                                                                      
    /**                                                                    
      * una sola funzione che esegue i comandi del programma su un ambiente
      *                                                                     
      * @param program lista di comandi da eseguire                                                 
      * @param env ambiente su cui eseguire i comandi                                                         
      */                                                                    
  def exec(program: Program, env: Environment): Unit = {                              
    def _exec(expressions: List[Comando], env: Environment): Unit = {                 
      expressions.foreach {                                                           
        case IncrementPointer() => env.incrementPointer()                             
        case DecrementPointer() => env.decrementPointer()                             
        case IncrementData() => env.increment()                                       
        case DecrementData() => env.decrement()                                       
        case Loop(innerExpressions) => while (env.get() > 0) _exec(innerExpressions, env)
        case Print() => print(env.get().toChar)                                       
        case PrintState() => println(env)                                             
        case Input() => env.put(Console.in.read())                          
        case _ => throw new IllegalArgumentException                        
      }                                                                     
    }                                                                       
    _exec(program.expressions, env)                                         
  }                                                                         
}                                                                           
                                                                                                                            
object brainfuck {                                                                                                          
    def main(args: Array[String]): Unit = {                                                                                 
                                                                                                                            
        //1 lettura di tutti i files dati in input                                                                          
        args.foreach{ filename =>                                                                                           
            val src = scala.io.Source.fromFile(filename)                                                                           
            val linee = src.mkString                                                                                        
                                                                                                                            
            //2 chiamata alla funzione parseAll dell'oggetto brainfuckInterprete                                                    
            brainfuckParser.parseAll(brainfuckParser.program, linee) match {                                                        
                case brainfuckParser.Success(t/*risultato del parser*/,_/*avanzo di quello che non è stato processato*/) =>   
                    //3 passaggio delle variabili ad un interprete                                                       
                    brainfuckInterpreter.exec(t, new Environment)                                                       
                                                                                                                        
                //4 nel caso il parsing dovesse fallire allora printo l'errore                                                        
                case fallimento => println(fallimento.toString)                                                         
            }                                                                                                           
                                                                                                                        
            src.close()                                                                                                 
        }                                                                                                               
    }                                                                                                                   
}                                                                                                                       