// esercizio riguardante la valdidazione di email:
import scala.util.parsing.combinator._

case class Mail(username: String, domain: String, tag: String)
case class Date(day: Int, month: Int, year: Int)

object mailParser extends JavaTokenParsers{
    def validate: Parser[Mail] = username ~ "@" ~ domain ~ (".com" | ".it" | ".net") ^^ {
      case u ~ at ~ d ~ tag => Mail(u, d, tag)

      case _ => throw new IllegalArgumentException("email non valida")
    }

    def username: Parser[String] = ident

    def domain: Parser[String] = ident
}

object dateParser extends JavaTokenParsers{
  def analyze: Parser[Date] = day ~ "/" ~ month ~ "/" ~ year ^^ {
    case d ~ _ ~ m ~ _ ~ y => 
                              if(d > 30 || d < 1) println("day is out of bound: " + d)
                              if(m > 12 || m < 1) println("month is out of bound: " + m)
                              if(y > 9999 || y < 0) println("year is out of bound: "+ y)
                              Date(d, m, y)
    case _ => throw new IllegalArgumentException("date wasnt expressed in the right format")
  }   

  def day: Parser[Int] = """\d{1,2}""".r ^^ {_.toInt}

  def month: Parser[Int] = """\d{1,2}""".r ^^ {_.toInt}

  def year: Parser[Int] = """\d{4}""".r ^^ {_.toInt} 

}


object main extends App{
  //lettura da riga di comando della email
  println("inserisci una email")
  val mail = io.StdIn.readLine

  mailParser.parseAll(mailParser.validate, mail) match {
      case mailParser.Success(res, _) => println("il parsing ha avuto successo, " + res)

      case mailParser.NoSuccess(err, _) => err
  }

  println("insert a date in the format dd/mm/yyyy")
  val data = io.StdIn.readLine
  dateParser.parseAll(dateParser.analyze, data) match{
    case dateParser.Success(res, _) => println("data creata: " + res)

    case dateParser.NoSuccess(error, _) => println("errore nel parser: " + error)
  }
}
