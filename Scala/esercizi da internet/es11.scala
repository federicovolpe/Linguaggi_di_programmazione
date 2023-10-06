import scala.math._
import scala.io.StdIn.readInt

class funzioni {
  //Write a Java program to print the area and perimeter of a circle
  val area = (raggio: Int) => { Pi * raggio * raggio}
  val perimetro = (raggio: Int) => {Pi * raggio *2}
}
object es11{
    def main(args: Array[String]): Unit = {
        print("inserisci il raggio del cerchio : ") 
        val raggio = readInt()
        val funzioni = new funzioni()
        println(s"perimetro : ${funzioni.perimetro(raggio)}")
        println(s"area : ${funzioni.area(raggio)}")               
    }
}
