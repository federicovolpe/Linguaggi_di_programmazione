class Funzioni {
  val between = (x : Int) => x > 100 && x < 200
  
}
//Write a Scala program to check two given integers whether 
//either of them is in the range 100..200 inclusive
object main{
    def main (args: Array[String]) : Unit = {
        val primo : Int= 10
        val secondo : Int = 100 
        val func : Funzioni = new Funzioni 
        (func.between(primo), func.between(secondo)) match{
            case (true,true) => println("entrambi i numeri sono giusti") 
            case (false,true) => println("solo il secondo numero è giusto") 
            case (true,false) => println("solo il primo numero è giusto") 
            case (false,false) => println("entrambi i numeri sono sbagliati") 
            case _ => println("fanculo panzone")
        }
    }
}
