import scala.io.StdIn.readInt

class es7 {
  def tabellina (numero: Int, moltiplicatore : Int = 0): Unit = moltiplicatore match {
    case 11 => ()
    case _ => {
      println(s"$moltiplicatore x $numero = ${moltiplicatore * numero}")
      tabellina(numero, moltiplicatore + 1)
    }
  }
}
object es7 {
    def main(args: Array[String]): Unit = {
        //scrivere un programma che dato un numero ritorni tutta la tabellina di quel numero
        val funzioni = new es7()
        print("inserisc il numero : ")
        val numero = readInt()
        funzioni.tabellina(numero)
    }
}
