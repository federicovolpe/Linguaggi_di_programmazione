import scala.util.parsing.combinator._
import scala.collection._

case class Contatto(nome: String, numero: Int)

object Rubrica{
   var contatti = List[Contatto]()

   def add (c: Contatto) {
     contatti.find(_.nome == c.nome) match {
       case Some(x) => println("contatto "+ c +" gia presente nella rubrica")
                       modifica(x)
       case None    => println("contatto "+ c +" inserito nella rubrica")
                       contatti = contatti :+ c
      }
   }

   def modifica(c: Contatto) {
      val indice = contatti.indexOf(c)
      contatti.updated(indice, c)
   }
  
   /**
    * function that controls if the contact is presente
    * after it prints the call 
    */
   def chiama(){ 
     chiama
   }
   def print(): List[Contatto] = {
      contatti 
   }
}

object callParser extends JavaTokenParsers {
    def contatti: Parser[List[Contatto]] = rep1(contatto)

    def contatto: Parser[Contatto] = ident ~ "," ~ wholeNumber ^^ {
        case nome ~  c ~ numero => Contatto(nome, numero.toInt)
        case _ => throw new IllegalArgumentException("errore nella identificazione del contatto")
    }

    
    
}

object telefoni{
  def main(args: Array[String]): Unit= {
    //reading from the file
      val elenco = io.Source.fromFile("call.txt").mkString
      callParser.parseAll(callParser.contatti, elenco) match {
        case callParser.Success(rubrica, _) => println("elenco contatti parsati: " + rubrica)
                                               rubrica.foreach{c => Rubrica.add(c)}
                                               println("rubrica creata: " + Rubrica.print())

        case callParser.NoSuccess(errore, _) => println("errore nel parser: " + errore)
      }
      
  }        
}
