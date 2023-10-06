class Funzioni {
  val test = (x : Int, y : Int, z : Int) => { x >= 20 && x <= 50 || y >= 20 && y <= 50 || z >= 20 && z <= 50}
}
/*Write a Scala program to check whether three given integer values 
are in the range 20..50 inclusive. Return true if 1 or more of them 
are in the said range otherwise false*/

object main{
    def main(args: Array[String]): Unit = {
        val primo : Int = 24
        val secondo : Int = 124
        val terzo : Int = 2
        val funzioni : Funzioni = new Funzioni
        println(funzioni.test(primo,secondo,terzo))
    }
}