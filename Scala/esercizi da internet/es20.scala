// Scala program to convert a decimal number to hexadecimal number
import scala.io.StdIn._

class funzioni {
  def hex (numero : Int, lista : List[String] = Nil): List[String] = {
    val codici : List[String] = List("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "A", "B", "C", "D", "E", "F")
    numero match{
        case 0 => lista
        case _ => {
            println(s"aggiungo ${codici(numero % 16)} a ${lista.foreach{print}} ") 
            hex( numero/16, (codici(numero % 16) :: lista ))
        }
    }
  }
}
object es20 {
    def main(args: Array[String]) : Unit = {
        print("inserisci il numero : ")
        val numero = readInt()
        val funzioni = new funzioni()
        funzioni.hex(numero).foreach(print)
    }
}
