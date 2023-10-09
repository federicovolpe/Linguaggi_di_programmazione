//scrivere un programma che presi due numeri ne restituisca il quoziente
import scala.io.StdIn.readInt

object es2{
    def main(args: Array[String]): Unit ={
        print("inserisci il numeratore : ")
        val numeratore = readInt()
        print("inserisci il denominatore: ")
        val denominatore = readInt()
        println(numeratore/denominatore)
    }
}