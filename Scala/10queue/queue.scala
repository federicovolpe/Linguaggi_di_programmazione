import scala.util.parsing.combinator._
import scala.collection.mutable.Queue

sealed trait Comando
case class Push(n : Int) extends Comando
case class Pop() extends Comando      
case class Find(n : Int) extends Comando
                                      
object Interpreter{                   
  val stack = new Queue[Int]()        
                                      
  def exec(comandi: List[Comando]) = comandi.foreach{ comando => comando match {
      case Push(n) => println("aggiunto: " + n)
                      stack.enqueue(n)
                      println("queue rimanente " + stack)
      case Pop() => println("prelevato: " + stack.dequeue())
      case Find(n) => println("trovato l'elemento: " + stack.find(x => x == n))
    }                                 
  }                                   
}                                     
                                      
object qParser extends JavaTokenParsers {
  def parse : Parser[List[Comando]] = rep1(comando)
                                      
  def comando : Parser[Comando] = ("push" | "pop" | "find") ~ opt(wholeNumber) ^^ {
    case "push" ~ Some(n) => Push(n.toInt)
    case "pop" ~ None => Pop()
    case "find" ~ Some(n) => Find(n.toInt)
    case _ => throw new IllegalArgumentException("comando non riconosciuto")
  }
}

object queue{
  def main(args :Array[String]): Unit = {
    //reading file 
    args.foreach{file =>
      val testo = io.Source.fromFile(file).mkString
      qParser.parseAll(qParser.parse, testo) match {
        case qParser.Success(parsato, _) => println("risultato parsato: " + parsato)
                                            Interpreter.exec(parsato)

        case qParser.NoSuccess(msg, _) => println("qualcosa è andato storto nel parsing")
      }
    }
  }
}
