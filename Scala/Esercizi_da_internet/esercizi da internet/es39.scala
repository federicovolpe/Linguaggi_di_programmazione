//Write a Java program to create and display unique three-digit number using 1, 2, 3, 4
        
class funzioni {
  def combinazioni (cifre : List[Int], lista : List[List[Int]] = Nil): List[List[Int]] = cifre match {
    case (x) => x
    case _ => lista :+ (cifre(0)::combinazioni(cifre.drop(1), lista))
  }
}

object es39 {
    def main(args: Array[String]) : Unit = {
        val funzioni = new funzioni()
        funzioni.combinazioni(List(1,2,3,4)).foreach{print()}
    }
}