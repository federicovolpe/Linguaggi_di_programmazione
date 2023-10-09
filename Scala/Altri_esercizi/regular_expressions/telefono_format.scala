import scala.util.parsing.combinator._

object main extends App{
  //reading from command line
  var input = io.StdIn.readLine()


  println(cambiaformato.parseAll(cambiaformato.standard, input).toString)

}

object cambiaformato extends JavaTokenParsers {

  def standard: Parser[String] = "(" ~ """\d{4}""".r ~ ")" ~ """\d{4}""".r ~ "-" ~  """\d{4}""".r  ^^ {case _ ~ p ~ _ ~ f ~ _ ~ s => "riconosciuto il numero: " + p +""+ f +""+ s
    case _ => "numero non riconosciuto"}

}
