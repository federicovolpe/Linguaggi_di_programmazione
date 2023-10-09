class `14.scala` {
  
}
class funzioni {
    val misura = (temperatura : Int) => {
        temperatura match{
            case i if (temperatura > 100) => println("temperatura troppo alta")
            case i if (temperatura < 100) => println("temperatura giusta")
            case _ => println("temperatura incomprensibile") 
        }
    }
}
object main {
    def main(args: Array[String]) : Unit = {
        val temperatura = 10
        val f = new funzioni
        f.misura(temperatura)
    }
}